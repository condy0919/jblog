# JBlog

Jekyll blog posts manager based on `tabulated-list`.

## Installation

Put `jblog.el` in your Emacs system. Add the following to your `.emacs`:

``` emacs-lisp
(require 'jblog)
(setq jblog-posts-directory (expand-file-name "~/blog/_posts")
      jblog-post-headers [("Date"       12 t)
                          ("Title"      36 t)
                          ("Categories" 25 t)
                          ("Tags"       25 t)]
      jblog-post-headers-format (string-join '("---"
                                               "layout: post"
                                               "title: %s"
                                               "categories: note"
                                               "tags: note"
                                               "---")
                                              "\n"))
```

Or use [use-package](https://github.com/jwiegley/use-package) with
[straight.el](https://github.com/raxod502/straight.el):

``` emacs-lisp
(use-package jblog
  :ensure t
  :straight (:host github :repo "condy0919/jblog")
  :commands jblog
  :custom
  (jblog-posts-directory (expand-file-name "~/blog/_posts"))
  (jblog-post-headers [("Date"       12 t)
                       ("Title"      36 t)
                       ("Categories" 25 t)
                       ("Tags"       25 t)])
  (jblog-post-headers-format (string-join '("---"
                                            "layout: post"
                                            "title: %s"
                                            "categories: note"
                                            "tags: note"
                                            "---")
                                          "\n")))
```

Then run `jblog` to get an overview of your blog posts.

## Keybindings

| Key          | Function                                             |
|--------------|------------------------------------------------------|
| <kbd>C</kbd> | `jblog-create` a new blog post                       |
| <kbd>D</kbd> | `jblog-delete` the blog post at point                |
| <kbd>s</kbd> | `jblog-search` keywords from date, title, ... fields |
| <kbd>g</kbd> | `jblog-refresh`                                      |
| <kbd>q</kbd> | `quit-window`                                        |

## Customizations

- `jblog-posts-directory` (Default: `nil`)
- `jblog-post-default-ext` (Default: `md`)
- `jblog-post-exts-regexp` (Default: a regexp matched with `md` and `markdown`)
- `jblog-post-headers` (Default: Only list `Date` and `Title` fields)
- `jblog-post-headers-format`
- `jblog-post-sort-key`

## Screenshots

![example](https://user-images.githubusercontent.com/4024656/86052277-51918a80-ba89-11ea-8c85-40998012bdcb.png)
