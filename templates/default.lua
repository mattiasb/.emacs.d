--[[

  Copyright (C) ${1:`(my/insert-year)`} ${2:`(if (boundp 'user-full-name) user-full-name "Author")`}

  ${3:$$(yas-choose-value (directory-files "~/.emacs.d/snippets/licenses/" nil "^[A-Za-z0-9-+_][A-Za-z0-9-+_.]*$"))}

  Author: ${4:$2} <${5:`(if (boundp 'user-mail-address) user-mail-address "user@example.com")`}>

--]]

---------------------------
-- Source initialization --
---------------------------

source = {
  id              = "`(file-name-sans-extension (buffer-name))`",
  name            = "$6",
  description     = "$7",
  supported_keys  = { "$8" },
  supported_media = "${9:$$(yas-choose-value '("none" "audio" "video" "image" "all"))}",
  tags            = { "$10" }
}

------------------
-- Source utils --
------------------

$0

