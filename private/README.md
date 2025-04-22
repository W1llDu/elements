## elements devloper documentation
This serves as a quick guide to the high level implementation of `elements`. To start,
the DSL is split into multiple files that handle different parts of the logic. 

## 1. Syntax and Macros (`main.rkt`)
The top level of the DSL is the syntax and macros the user can use to define different weapons, skills, characters, etc.
This also includes the macros that allow users to run calculations, and either return the raw data, or display the data
in a formatted manner that also checks previous saved data.

## 2. Compiler (`compile.rkt`)
While binding classes ensure the proper definitions are put in the right spots, the user written code is also checked
to ensure no duplicate names were defined. Technically, this is done without the compilers help, but the compiler allows
for better/more helpful error messages, which is why we still include it.

## 3. Calculating Damage and Time (`runtime.rkt`)
Once the code is properly checked, the actual calculations occur.
In this file, the data is partially flattened to make accessing data easier for calculation purposes.
Then, the overall damage and time are updated over time, tracking the current character, current attack, and active buffs.
Once the rotation ends, the final result represents the damage done along with the time it took to do the rotation. 

## 4. Saving and Loading Data (`save-data.rkt`)
Calculated data is saved into a data file, which can be read from later. This is useful in the event data needs to be displayed,
in which comparisons to previous runs are made. 

## 5. Displaying Calculations (`display.rkt`)
Runtime data is then converted into a displayable format in this file. This includes specifying what the returned data means,
and rounding results to be more readable. Additionally, checks are made to determine if the current run is the most optimal, displaying
the highest performing rotation to the user as well. If needed, the user can also choose to forgo this step and simply print the raw data.



