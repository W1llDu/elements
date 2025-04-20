## elements: A DSL for optimizing and calculating Genshin Impact damage rotations

[Genshin Impact](https://genshin.hoyoverse.com/en/) is an open world RPG released in 2020. In Genshin, you control a team of up to four characters, and engage in various levels of combat.
The goal is typically to clear content as fast as possible, which requires efficient team rotations with high levels of sustained damage.
This package implements a damage calculator as a hosted DSL. With `elements`, you can easily define the basics of a team lineup, including weapons, elemental skills, and characters.
From there, you can setup damage rotations against enemies of varying toughness, and calculate potential damage output. Additionally, `elements` compares old rotations to newer ones,
allowing you to easily keep track of what combination has provided the highest sustained damage. 
