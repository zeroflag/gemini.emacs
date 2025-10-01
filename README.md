## Gemini Chat Bot for Emacs

```bash
echo "<YOUR-API-KEY>" > .api-key.txt
```

### config.el

```lisp
(use-package! gemini
  :load-path "~/path/to/gemini.emacs/"
  :commands gemini-interactive-send
  :defer t
  :init
  (map! :leader
        :desc "Send buffer to Gemini Assistant"
        "g g" #'gemini-interactive-send))
```
