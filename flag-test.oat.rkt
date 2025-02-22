# start
set the flag?
@set [set the flag]
@dont-set [dont set the flag]

# set
(set-flag the-flag)
setting the flag
@cont [ok]

# dont-set
not setting the flag
@cont [ok]


#? cont {the-flag}
the flag was set
@@end
---
the flag was not set
@@end
