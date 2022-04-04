;; Settings for my ERC IRC Client

;; Set our nickname & real-name as constant variables
(setq
 erc-nick "greggo"     ; Our IRC nick
 erc-user-full-name "Greggo My Eggo") ; Our /whois name

;; Define a function to connect to a server
(defun libera-serv ()
  (lambda ()
    (interactive)
    (erc :server "irc.libera.chat"
         :port   "6667")))

(require 'easymenu)
(easy-menu-add-item  nil '("tools")
                     ["IRC with ERC" erc t])
