(use-modules (guix packages)
             (guix gexp)
             (guix git-download)
             (guix build-system guile)
             (guix build-system copy)
	     (gnu packages guile)
             (guix licenses))

(define-public dotstow
  (package
    (name "dotstow")
    (version "0.3")
    (source
     (origin
       (method git-fetch)
       (uri
	(git-reference
	 (url "https://github.com/icot/dotstow.git")
	 (commit version)))
       (file-name (git-file-name name version))
       (sha256 (base32 "1qdvgcchnk6r6v2kvw8ggbb5b19h97k9k6kk909lkppd2xs0sxw8"))))
    (inputs
     (list guile-3.0))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("dotstow.scm" "bin/dotstow"))))
    (synopsis "Personal dotfiles stow tool")
    (description "Like GNU STOW, more basic, with naive template support. Only for dotfiles")
    (home-page "https://github.com/icot/dotstow")
    (license gpl3+)))

dotstow

