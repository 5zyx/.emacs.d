# EPUB 阅读与摘录笔记 - 设计文档

**日期**: 2026-09-11
**状态**: 已确认，待实现
**范围**: 单文件改动 (`custom-post.el`)，约 60 行

---

## 目标

在 Centaur Emacs 中阅读 EPUB 电子书，通过 org-capture 做摘录与读书笔记，并用 org-noter 进行章节精读。

## 现状

| 组件 | 现状 | 缺口 |
|------|------|------|
| EPUB 阅读 | `lisp/init-reader.el` 已配置 `nov.el`（nov-mode，`M-<f7>` 阅读模式） | 无缺口 |
| org-capture | `lisp/init-org.el` 已有模板（`b` Book → book.org datetree） | 缺少能提取 EPUB 元数据的摘录模板 |
| org-roam | `lisp/init-org.el` 已启用（`C-c n f/c/i` 等） | 无缺口 |
| org-noter | 未安装 | 需新增 `use-package org-noter` |

## 设计决策（用户已确认）

1. **工作流**：capture 模板 + org-noter 两者结合
2. **笔记存储**：每本书一个 org-roam 节点（`notes/<书名>.org`），org-noter 挂载到该节点
3. **capture 模板**：统一模板（引文 + 读书笔记合一）

## 实现方案

### 文件：`custom-post.el`（全部改动集中于此）

符合 AGENTS.md：用户 SHOULD 修改 `custom-post.el`，MUST NOT 修改 `lisp/init-*.el`。

### 组件

#### 1. EPUB 元数据提取 helpers

**关键约束**：org-capture 的 `%(sexp)` 在 capture 临时 buffer（org-mode）中求值，而非原始 nov buffer（见 `org-capture.el:1847,1899`，`org-capture-expand-embedded-elisp`）。因此 helpers 必须通过 `(org-capture-get :original-buffer)` 取到原始 nov buffer，再 `with-current-buffer` 读取其 buffer-local 变量。

```elisp
(defun my/nov--context ()
  "Return context alist from capture's original buffer when in `nov-mode'.
org-capture evaluates %(sexp) in a temp org buffer, so we must fetch the
original buffer via `org-capture-get'.  Falls back to current buffer when
not capturing (e.g. interactive testing)."
  (let ((buf (or (and (fboundp 'org-capture-get)
                      (let ((ob (org-capture-get :original-buffer)))
                        (and (buffer-live-p ob) ob)))
                 (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (and (derived-mode-p 'nov-mode)
                   (boundp 'nov-metadata) nov-metadata)
          `((title   . ,(or (cdr (assq 'title nov-metadata)) "未知书名"))
            (author  . ,(or (cdr (assq 'creator nov-metadata)) "未知作者"))
            (file    . ,(or (and (boundp 'nov-file-name) nov-file-name) ""))
            (chapter . ,(my/nov--current-chapter))))))))

(defun my/nov--current-chapter ()
  "Extract current chapter from `header-line-format' (nov renders \"书名: 章节\").
Fallback to \"第 N 章\" using `nov-documents-index'."
  (let ((hl (and (local-variable-p 'header-line-format) header-line-format)))
    (if (and hl (stringp hl) (string-match ":" hl))
        (let ((parts (split-string hl ":")))
          (if (> (length parts) 1)
              (string-trim (mapconcat #'identity (cdr parts) ":"))
            (my/nov--fallback-chapter)))
      (my/nov--fallback-chapter))))

(defun my/nov--fallback-chapter ()
  "Fallback chapter label from `nov-documents-index'."
  (format "第 %d 章"
          (1+ (or (and (boundp 'nov-documents-index) nov-documents-index) 0))))

;; 模板可调用的访问器（返回字符串，非 epub 下安全回退 ""）
(defun my/nov-context-title   () (or (cdr (assq 'title   (my/nov--context))) ""))
(defun my/nov-context-author  () (or (cdr (assq 'author  (my/nov--context))) ""))
(defun my/nov-context-chapter () (or (cdr (assq 'chapter (my/nov--context))) ""))
(defun my/nov-context-headline ()
  "Capture headline: \"书名 — 章节\" in nov, \"摘录\" otherwise."
  (let ((ctx (my/nov--context)))
    (if ctx
        (format "%s — %s" (cdr (assq 'title ctx)) (cdr (assq 'chapter ctx)))
      "摘录")))
```

nov.el API 依据：
- `nov-metadata`：buffer-local alist，keys 含 `title`、`creator`（作者，OPF 标准）、`subject`、`date`、`publisher`
- `nov-file-name`：buffer-local，epub 文件路径
- 章节标题：nov 在 `nov-render-title`（nov.el:588）将 `%c` 替换为当前文档 `<title>` 子节点并写入 `header-line-format`；无独立变量。解析 `header-line-format`（split on `:`，取首冒号之后并 trim），失败回退到 `nov-documents-index + 1`。

#### 2. org-noter 配置

```elisp
(use-package org-noter
  :ensure t
  :after org
  :bind (:map nov-mode-map
         ("C-c n n" . org-noter))  ; 在 epub 中按此键启动/复用 noter 会话
  :custom
  (org-noter-notes-search-path (list centaur-org-directory))
  (org-noter-default-notes-mode 'outline-mode))
```

#### 3. 统一摘录/笔记 capture 模板

追加到 `org-capture-templates`（`"e"` Excerpt）：

```elisp
(add-to-list 'org-capture-templates
  '("e" "Excerpt & Note" entry
    (file (lambda () (concat centaur-org-directory "/notes.org")))
    "* %(my/nov-context-headline) :excerpt:
:PROPERTIES:
:BOOK:    %(my/nov-context-title)
:AUTHOR:  %(my/nov-context-author)
:CHAPTER: %(my/nov-context-chapter)
:END:
#+begin_quote
%i
#+end_quote

%?

%a" :empty-lines 1 :jump-to-captured t))
```

说明：
- `%i` = 选中的文本（nov-mode 中选中的段落）
- `%a` = capture 位置链接（链接回 epub buffer/位置）
- `%(...)` = 动态求值，调用 helper 提取 epub 元数据
- 非 epub buffer 下调用时 helper 返回安全回退值，模板仍可用

#### 4. nov-mode 快捷键

- `C-c o x` 已是全局 org-capture（无需改动）
- 在 `nov-mode-map` 绑定 `C-c n n` → org-noter（启动精读会话）

### 目录结构

```
centaur-org-directory/        (即 org-roam-directory，已有)
├── notes.org                 (摘录模板目标文件，org-roam 自动纳入)
├── <书名>.org                 (org-noter 深读笔记文件，org-roam 节点)
├── idea.org                  (已有)
├── gtd.org                   (已有)
├── book.org                  (已有 datetree 模板，保留)
└── journal.org               (已有)
```

## 数据流

### 摘录流程
```
nov-mode 选中文字
  → C-c o x → 选 "e"
  → my/nov-capture-context 提取 title/author/chapter
  → capture 弹出，%i 插入引文为 #+begin_quote
  → 用户写 %? 笔记
  → C-c C-c 保存到 notes.org 子节点
  → org-roam-db-autosync 自动索引（反链/图谱可用）
```

### 精读流程
```
C-c n f 搜索/创建书名 org-roam 节点
  → 打开该节点 buffer
  → M-x org-noter（或 nov 中 C-c n n）
  → 左右分屏：epub | 笔记 buffer
  → 笔记子标题自动锚定 epub 当前位置
  → 点笔记标题 → 跳回书中对应位置
```

## 边界与回退

- 非 epub buffer 触发 `"e"` 模板：helper 返回空字符串回退，不报错，模板仍可用
- `nov-metadata` 中 author 键名是 `creator`（OPF 标准），多作者时为逗号分隔字符串
- org-noter 需要 epub 文件路径；nov-mode 下 `nov-file-name` 提供

## 不做（YAGNI）

- 不修改 `lisp/init-reader.el` 或 `lisp/init-org.el`
- 不引入 org-pdftools / pdf 笔记集成（本任务只管 epub）
- 不实现自定义 org-noter 样式

## 验证

- byte-compile `custom-post.el` 无警告
- 打开一个 epub，选中文字 → `C-c o x` → `e` → 验证引文/元数据正确
- `M-x org-noter` 启动会话 → 验证分屏 + 位置锚定
- `C-c n f` 搜索书名 → 验证 org-roam 节点可见
