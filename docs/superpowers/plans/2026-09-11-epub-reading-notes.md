# EPUB 阅读与摘录笔记 实施计划

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** 在 Centaur Emacs 中阅读 EPUB 并通过 org-capture 做摘录/读书笔记，用 org-noter 做章节精读。

**Architecture:** 全部改动追加到 `custom-post.el`（符号链接至 `~/.dotfiles/emacs-custom/custom-post.el`，经 after-init-hook 加载）。新增 5 个 helper 函数（从 capture 原始 buffer 提取 nov 元数据）、`use-package org-noter` 配置、一个 `"e"` capture 模板。不修改任何 `lisp/init-*.el`。

**Tech Stack:** Emacs Lisp（lexical-binding）、nov.el（EPUB reader，已装）、org 9.8（capture）、org-roam（已启用）、org-noter（待装）。

**测试方式:** 本仓库无自动化测试（见 AGENTS.md "No Automated Tests"）。验证用 `emacsclient` 做 byte-compile + `check-parens`，加手动功能验证（打开 epub → capture → org-noter）。AGENTS.md 要求只用 `emacsclient`，不用 `emacs`/`--batch`。

---

## 关键设计要点（实现时务必遵守）

1. **`%(sexp)` 求值上下文**：org-capture 在临时 org buffer 中求值 `%(sexp)`（`org-capture.el:1847,1899`），当前 buffer **不是** nov buffer。因此 helpers 必须用 `(org-capture-get :original-buffer)` 取原始 nov buffer 再 `with-current-buffer` 读其 buffer-local 变量（`nov-metadata`、`nov-file-name`、`header-line-format`、`nov-documents-index`）。
2. **nov.el API**：`nov-metadata` 是 alist，作者键名为 `creator`（OPF 标准），书名键为 `title`；`nov-file-name` 为 epub 路径；章节标题无独立变量，渲染进 `header-line-format`（`nov-render-title`, nov.el:588）。
3. **键绑定 `C-c n n`**：nov-mode-map 原本无 `C-c` 绑定；`C-c n n` 不会破坏全局 org-roam 的 `C-c n f`（完整序列查找会落到全局 map），但须在验证步骤确认。
4. **不改 core 文件**：符合 AGENTS.md（用户 SHOULD 改 custom-post.el，MUST NOT 改 lisp/init-*.el）。

---

### Task 1: 追加 helper 函数到 custom-post.el

**Files:**
- Modify: `~/.dotfiles/emacs-custom/custom-post.el`（即 `~/.emacs.d/custom-post.el` 软链接目标），追加到文件末尾（第 109 行 `my/markdown-electric-pair-setup` 的 `(add-hook ...)` 之后）

- [ ] **Step 1: 读取文件确认末尾内容**

Run: `emacsclient --eval '(progn (find-file "~/.dotfiles/emacs-custom/custom-post.el") (goto-char (point-max)) (forward-line -2) (buffer-substring (line-beginning-position) (line-end-position)))'`
Expected: 返回 `(dolist (hook '(markdown-mode-hook gfm-mode-hook)) ...)` 所在行或 `(add-hook hook #'my/markdown-electric-pair-setup)` —— 确认这是文件末尾的最后一行。

- [ ] **Step 2: 追加 helper 函数代码块**

在文件末尾追加以下内容（含一个分节注释头与 5 个函数，docstring 已含用法说明，不额外加行内注释）：

```elisp


;;; EPUB 阅读与摘录笔记
;; 用法：选中文字 C-c o x → e 摘录笔记；M-x org-noter 或 nov 中 C-c n n 章节精读

(defun my/nov--context ()
  "Return context alist from capture's original buffer when in `nov-mode'.
org-capture evaluates %(sexp) in a temp org buffer, so fetch the original
buffer via `org-capture-get' and read its buffer-local variables."
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
  "Extract current chapter title from `header-line-format'.
nov.el renders the header line as \"书名: 章节标题\" (see `nov-render-title').
Split on the first colon; fallback to \"第 N 章\" from `nov-documents-index'."
  (let ((hl (and (local-variable-p 'header-line-format) header-line-format)))
    (if (and hl (stringp hl) (string-match ":" hl))
        (let ((parts (split-string hl ":")))
          (if (> (length parts) 1)
              (string-trim (mapconcat #'identity (cdr parts) ":"))
            (my/nov--fallback-chapter)))
      (my/nov--fallback-chapter))))

(defun my/nov--fallback-chapter ()
  "Fallback chapter label using `nov-documents-index'."
  (format "第 %d 章"
          (1+ (or (and (boundp 'nov-documents-index) nov-documents-index) 0))))

(defun my/nov-context-title   () (or (cdr (assq 'title   (my/nov--context))) ""))
(defun my/nov-context-author  () (or (cdr (assq 'author  (my/nov--context))) ""))
(defun my/nov-context-chapter () (or (cdr (assq 'chapter (my/nov--context))) ""))

(defun my/nov-context-headline ()
  "Capture headline: \"书名 — 章节\" inside `nov-mode', \"摘录\" otherwise."
  (let ((ctx (my/nov--context)))
    (if ctx
        (format "%s — %s" (cdr (assq 'title ctx)) (cdr (assq 'chapter ctx)))
      "摘录")))
```

- [ ] **Step 3: 检查括号匹配**

Run: `emacsclient --eval '(with-temp-buffer (insert-file-contents "~/.dotfiles/emacs-custom/custom-post.el") (check-parens))'`
Expected: 无输出（无错误），表示括号平衡。若有错，定位修复。

- [ ] **Step 4: byte-compile 文件**

Run: `emacsclient --eval '(byte-compile-file "~/.dotfiles/emacs-custom/custom-post.el")'`
Expected: 无 `Error` / `Warning` 输出（关于新函数）。注意：`nov-metadata` 等是运行时 buffer-local 变量，byte-compiler 可能报 "free variable" 警告 —— 若仅此类警告可接受（运行时正常）。确认无 `*** byte-compile error`。

---

### Task 2: 追加 org-noter 配置与 capture 模板

**Files:**
- Modify: 同 `~/.dotfiles/emacs-custom/custom-post.el`，接 Task 1 末尾继续追加

- [ ] **Step 1: 追加 org-noter use-package 与 capture 模板**

接 Task 1 追加的代码末尾，继续追加：

```elisp

(use-package org-noter
  :ensure t
  :after org
  :bind (:map nov-mode-map
         ("C-c n n" . org-noter))
  :custom
  (org-noter-notes-search-path (list centaur-org-directory)))

(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
    `("e" "Excerpt & Note" entry
      (file+olp+datetree ,(concat centaur-org-directory "/notes.org"))
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

%a" :empty-lines 1 :jump-to-captured t)))
```

说明：
- 模板用反引号以便 `,` 插值 `centaur-org-directory`（custom-post 在 after-init 加载，此时 `centaur-org-directory` 已定义）。
- `%(my/nov-context-headline)` 等在 capture 时求值（Task 1 helpers 已用 `:original-buffer` 正确取数）。
- `%i` = 选中文字，`%a` = 指回 epub 位置的链接。
- `"e"` 键与现有 `i/t/n/j/b` 不冲突。
- `with-eval-after-load 'org-capture` 保证模板加入时机正确。

- [ ] **Step 2: 检查括号匹配**

Run: `emacsclient --eval '(with-temp-buffer (insert-file-contents "~/.dotfiles/emacs-custom/custom-post.el") (check-parens))'`
Expected: 无错误。

- [ ] **Step 3: byte-compile 文件**

Run: `emacsclient --eval '(byte-compile-file "~/.dotfiles/emacs-custom/custom-post.el")'`
Expected: 无 byte-compile error。free-variable 警告（`centaur-org-directory`、`org-capture-templates`）可接受（运行时已绑定）。

- [ ] **Step 4: 重新加载 custom-post 验证无加载错误**

Run: `emacsclient --eval '(load "~/.dotfiles/emacs-custom/custom-post.el" nil t)'`
Expected: 无 `error` / `void-variable` / `void-function`，返回 `t`。若报 `org-noter-notes-search-path` void（说明变量名有误），改用 `:config` 内 `(setq org-noter-notes-search-path (list centaur-org-directory))` 包在 `(when (boundp ...) ...)` 守卫中，或移除该 `:custom`（org-noter 默认行为可用 org-roam 目录）。

---

### Task 3: 手动功能验证

**Files:**
- 无文件改动，纯验证

- [ ] **Step 1: 准备一个测试 epub**

若无现成 epub，下载一个公开领域 epub（如 PGutenberg）到本地。命令示例（macOS）：

Run: `curl -sL -o /tmp/test.epub "https://www.gutenberg.org/ebooks/1342.epub.images"` && `ls -lh /tmp/test.epub`
Expected: 出现 `test.epub` 文件。

- [ ] **Step 2: 在 emacsclient 中打开 epub**

Run: `emacsclient --eval '(find-file "/tmp/test.epub")'`
Expected: buffer 进入 `nov-mode`，可见正文渲染。

- [ ] **Step 3: 验证 helper 函数返回正确元数据**

Run: `emacsclient --eval '(with-current-buffer (get-buffer "test.epub") (my/nov--context))'`
Expected: 返回 alist，含 `("title" . "...")`、`("author" . "...")`、`("chapter" . "第 1 章")`（或解析出的章节名）。若返回 nil，确认 buffer 名为 `test.epub`（用 `(buffer-list)` 查）。

- [ ] **Step 4: 验证 capture 模板已注册**

Run: `emacsclient --eval '(assoc "e" org-capture-templates)'`
Expected: 返回 `"e"` 模板的列表（非 nil）。

- [ ] **Step 5: 实际触发 capture（交互）**

在 emacs 中：打开 epub → 选中一段文字 → `C-c o x` → 选 `e` → 确认弹出的 capture 中：标题行为 `书名 — 章节`，PROPERTIES 的 BOOK/AUTHOR/CHAPTER 已填，`#+begin_quote` 内为选中文字，`%?` 处可写笔记 → `C-c C-c` → 确认写入 `notes.org` 的当日 datetree 下。

- [ ] **Step 6: 验证 `C-c n n` 不破坏 org-roam 键**

在 epub buffer 中按 `C-c n f`：预期弹出 org-roam-node-find（即全局 org-roam 键仍生效）。再按 `C-c n n`：预期启动 org-noter（首次会提示创建/选笔记文件）。若 `C-c n f` 失效（undefined），将 `C-c n n` 改为 `C-c N`（大写 N，用户保留键，无前缀冲突）后重启验证。

- [ ] **Step 7: 验证 org-noter 分屏与位置锚定**

在 epub 中 `C-c n n` 启动 org-noter → 确认左右分屏（epub | 笔记 buffer）→ 在笔记 buffer 加一条 `** 测试笔记` → 移动 epub 到另一处 → 确认新笔记锚定新位置 → 在笔记条目上按 org-noter 跳转键跳回对应 epub 位置。

---

## 自检（Self-Review）

**1. Spec 覆盖**：设计文档要求的 4 部分——helpers（Task 1）、org-noter（Task 2）、capture 模板（Task 2）、nov 键绑定（Task 2）——均有任务实现。✓

**2. 占位符扫描**：无 TBD/TODO；所有代码步骤含完整代码。✓ Task 2 Step 4 含一个条件分支（变量名错误时的修复），这是验证后的可能修复，非占位符。

**3. 类型/名称一致性**：Task 1 定义 `my/nov-context-headline/title/author/chapter`；Task 2 模板引用同名函数。✓ `my/nov--context`、`my/nov--current-chapter`、`my/nov--fallback-chapter` 定义与调用一致。✓ `centaur-org-directory`、`org-capture-templates`、`nov-mode-map` 为已有/包内符号。✓

**4. 求值上下文**：Task 1 helpers 用 `org-capture-get :original-buffer` 而非依赖当前 buffer（已在"关键设计要点"第 1 条强调）。✓
