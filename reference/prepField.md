# Prepare Functional Field

This function will create the layout for the functional field matrix,
and help to maintain consistency between the field matrix and graphical
elements of the model. It will also specify the type of each variable in
the graph - especially whether it should be represented as an *action*,
*object*, *variable*, *choice point*, or *appraisal*.

Variables in the functional field should be specified using a
'type_name' labeling convention. Please see the **\*\*as yet unwritten
help file\*\*** for further details on preparing a matrix layout.

## Usage

``` r
prepField(matLayout)
```

## Arguments

- matLayout:

  Specified *variable type* and *variable name*

## Value

Various objects that will be useful for creating fields from a
prespecified matrix

## Details

Current (main) variable types: `d` = decision or 'do' nodes (indicate
initiating some sort of action) `o` = object nodes - should only take
values of 1 or 0 `c` = choice point / chance nodes `x` = continuous
variables `a` = appraisal nodes - indicate where a decision will be made
through a maximization or threshold rule
