;;; gpt-commit.el --- GPT Conventional Commit Title -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A hook using GPT
;; (require 'gpt-commit)
;; (setq gpt-commit-openai-key "YOUR_OPENAI_API_KEY")
;; (setq gpt-commit-model-name "gpt-3.5-turbo-16k")
;; (add-hook 'git-commit-setup-hook 'gpt-commit-message)


;;; Code:

(provide 'gpt-commit)

(require 'magit)
(require 'request)

(defvar gpt-commit-openai-key nil "API key for the OpenAI.")
(defvar gpt-commit-model-name "gpt-3.5-turbo"
  "Model name to use for GPT chat completions.")

(defconst gpt-api-url "https://api.openai.com/v1/chat/completions"
  "API endpoint for GPT chat completions.")


(defconst gpt-commit-system-prompt-en
  "The user provides the result of running `git diff --cached`. You suggest a conventional commit message. Don't add anything else to the response. The following describes conventional commits.

# Conventional Commits 1.0.0

## Summary

The Conventional Commits specification is a lightweight convention on top of commit messages.
It provides an easy set of rules for creating an explicit commit history;
which makes it easier to write automated tools on top of.
This convention dovetails with [SemVer](http://semver.org),
by describing the features, fixes, and breaking changes made in commit messages.

The commit message should be structured as follows:

---

```
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```
---

<br />
The commit contains the following structural elements, to communicate intent to the
consumers of your library:

1. **fix:** a commit of the _type_ `fix` patches a bug in your codebase (this correlates with [`PATCH`](http://semver.org/#summary) in Semantic Versioning).
1. **feat:** a commit of the _type_ `feat` introduces a new feature to the codebase (this correlates with [`MINOR`](http://semver.org/#summary) in Semantic Versioning).
1. **BREAKING CHANGE:** a commit that has a footer `BREAKING CHANGE:`, or appends a `!` after the type/scope, introduces a breaking API change (correlating with [`MAJOR`](http://semver.org/#summary) in Semantic Versioning).
A BREAKING CHANGE can be part of commits of any _type_.
1. _types_ other than `fix:` and `feat:` are allowed, for example [@commitlint/config-conventional](https://github.com/conventional-changelog/commitlint/tree/master/%40commitlint/config-conventional) (based on the [Angular convention](https://github.com/angular/angular/blob/22b96b9/CONTRIBUTING.md#-commit-message-guidelines)) recommends `build:`, `chore:`,
  `ci:`, `docs:`, `style:`, `refactor:`, `perf:`, `test:`, and others.
1. _footers_ other than `BREAKING CHANGE: <description>` may be provided and follow a convention similar to
  [git trailer format](https://git-scm.com/docs/git-interpret-trailers).

Additional types are not mandated by the Conventional Commits specification, and have no implicit effect in Semantic Versioning (unless they include a BREAKING CHANGE).
<br /><br />
A scope may be provided to a commit's type, to provide additional contextual information and is contained within parenthesis, e.g., `feat(parser): add ability to parse arrays`.")

(defconst gpt-commit-system-prompt-ko
  "사용자는 `git diff --cached`의 실행 결과를 제공합니다. 당신은 conventional commit message를 제안합니다. 다음은 conventional commits에 대한 설명입니다.")

(defun gpt-commit-parse-response (data)
  "Parse the GPT response DATA."
  (let* ((choices (cdr (assoc 'choices data)))
         (choice (elt choices 0))
         (message (assoc 'message choice))
         (content (cdr (assoc 'content message))))
    (decode-coding-string content 'utf-8)))

(defun gpt-commit-openai-chat-completions-api (messages callback)
  "Call OpenAI's Chat Completions API with MESSAGES and return the response."
  (let* ((headers `(("Content-Type" . "application/json")
                    ("Authorization" . ,(concat "Bearer " gpt-commit-openai-key))))
         (json-string (json-serialize `((model . ,gpt-commit-model-name)
                                        (messages . ,messages))))
         (payload (encode-coding-string json-string 'utf-8)))
    (request gpt-api-url
             :type "POST"
             :headers headers
             :data payload
             :parser 'json-read
             :timeout 10
             :success
             (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback (gpt-commit-parse-response data))))
             :error
             (cl-function
              (lambda (&rest args &key data error-thrown &allow-other-keys)
                (message "Error: %s %s" error-thrown data))))))

(defun gpt-commit-generate-message (callback)
  (let* ((lines (magit-git-lines "diff" "--cached"))
         (changes (string-join lines "\n"))
         (messages `[((role . "system")
                      (content . ,gpt-commit-system-prompt-en))
                     ((role . "user")
                      (content . ,changes))]))
    (gpt-commit-openai-chat-completions-api messages callback)))

(defun gpt-commit-message ()
  "Automatically generate a commit message using GPT."
  (interactive)
  (let ((buffer (current-buffer)))
    (gpt-commit-generate-message
     (lambda (commit-message)
       (when commit-message
         (with-current-buffer buffer
           (insert commit-message)))))))

;;; gpt-commit.el ends here
