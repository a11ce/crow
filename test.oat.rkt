# fox-says-hi
yip yip hi im a fox
@name [what is your name miss fox]

# name
*fox noises!*
@bite [bite the fox]
@take [take the fox and put her in ur backpack]

# bite
(set-flag bitten)
*ow*
*the fox tastes really good*
@bite-again [bite the fox again]
@take [take the fox and put her in ur backpack]

# bite-again
chomp
@bite-again [bite the fox again]
@take [take the fox and put her in ur backpack]

#? take {bitten}
yay adventure
@end
---
yay adventure *bites you*
[[GAME TIP: you can tame a fox by biting it]]
@end

#? end {bitten}
(set-image sunset-purple)
you and the fox head off happily ever after
---
(set-image sunset-red)
you and the fox head off happily ever after
