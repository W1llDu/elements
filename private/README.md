## Elements developer documentation
This serves as a quick guide to the high level implementation of `Elements`. To start,
the DSL is split into multiple files that handle different parts of the logic. 

### 1. Syntax and Macros (`main.rkt`)
The top level of the DSL is the syntax and macros the user can use to define different weapons, skills, characters, etc.
This also includes the macros that allow users to run calculations, getting either data in its raw form or displayed on the terminal providing additional info by checking previously saved data.

### 2. Compiler (`compile.rkt`)
While binding classes ensure the proper definitions are put in the right spots, the user written code is also checked
to ensure no duplicate names were defined. Technically, this is done without the compilers help, but the compiler allows
for better/more helpful error messages, which is why we still include it.

### 3. Calculating Damage and Time (`runtime.rkt`)
Once the code is properly checked, the actual calculations occur.
The loop goes as follows:
Determine the initial state, flattening structs to make them easier to work with.
- Instead of multiple artifacts/weapons, collect all the buffs they grant into a flat list
- Give applied buffs their respective triggers (attacks for weapons, skill use for skill/burst)
For each action:
- Update the state, and determine the next state
  - Update/reset the normal attack counter (`determine-next-nc`)
  - Apply buffs, and remove buffs that expired (in two steps) (`add-new-triggered`, `remove-expiring-buffs`)
  - Update the enemy element (`determine-enemy-element`)
- Calculate the damage:
  - Calculate the stats of the character at this moment (after buffs are applied and before they expire) (`calc-total-stats`)
  - Calculate the values for use in the [general damage formula](https://genshin-impact.fandom.com/wiki/Damage) (`calc-*`)
  - Get the final damage of the attack from the formula (`determine-damage`)
- Calculate the time the action took (`calc-duration`)
Add these values to the accumulator, and recurse on the rest of the actions with the updated state (e.g. process buff expiry/application).
Once there are no more actions, return the damage done and how much time it took, and save the result.

### 4. Saving and Loading Data (`save-data.rkt`)
Calculated data is saved into a data file, which can be read from later. This is useful in the event data needs to be displayed,
in which comparisons to previous runs are made.

### 5. Displaying Calculations (`display.rkt`)
Runtime data is then converted into a displayable format in this file. This includes specifying what the returned data means,
and rounding results to be more readable. Additionally, checks are made to determine if the current run is the most optimal, displaying
the highest performing rotation to the user as well. If needed, the user can also choose to forgo this step and simply print the raw data.



