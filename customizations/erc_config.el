;; Settings for my ERC IRC Client

;; Set our nickname & real-name as constant variables
(setq 
 erc-server "irc.libera.chat"
 erc-nick "greggo"
 erc-user-full-name "Greggo My Eggo"
 erc-prompt (lambda () (concat "[" (buffer-name) "] " ))
 erc-auto-query 'bury
 erc-fill-column 100
 erc-fill-function 'erc-fill-static
 erc-fill-static-center 22
 )

(require 'easymenu)
(easy-menu-add-item  nil '("tools")
                     ["IRC with ERC" erc t])
