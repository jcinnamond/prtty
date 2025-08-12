# prtty

`prtty` is a presentation tool that runs in your terminal.

It lets you define your using the Presentation Definition Language (PDL), and provides a simple runtime for executing the presentation in a terminal.

A presentation might look something like this:

```
    \bg = #000000
    \fg = #ffffff

    .prelude
        .margin [ left = 10% ; top = 10% ]
        .style [ fg = $fg; bg = $bg ]

    .slide
        .middle
            .style[bold]
                Presentation title

    .slide
        .vcenter
            This presentation consists of

            .vspace[lines=2]
            .list[bullet="  *"]
                An introduction
                An problem statement
                A proposed solution
                A conclusion
```

## Installing
Right now you need the latest Haskell compiler (>=9.12.0) to build the executable.

To build the binary run
    cabal build

If you use nix you can run `nix develop` to get a development environment with everything you need.

## The path to 1.0
`prtty` is currently alpha quality. It works well enough for me to use right now, but it has a lot of rough edges and it might undergo major changes. The presentation markup languge should be considered unstable and liable to change.

To get to a stable release I'd like to finish the following:

### Major changes
#### Custom function definition in PDL
Maybe something like
```
\middle [...] -> 
    .vcenter
        .center
            ...

.slide
    .middle
        Text in the middle of the slide
```

#### Inline styling in text literals
I current write this a lot and it's awkward
```
This has an 
.style[fg=$highlight] << emphasised 
word
```

Ideally I'd have something like
```
This has an {{style[fg=$highlight]: emphasised}} word
```

#### Fix `center` so that it distributes across multiple lines
e.g., something like this should work
```
.center
    > Line 1
    > Line 2
    > Line 3
```

#### Add a proper type checker
Some basic type checking happens during the compilation but it's ad-hoc and unreliable.

### Minor changes
#### Support vertical alignment arguments to `slide`
```
.slide[valign=middle]
```