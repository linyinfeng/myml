# myml

[![Build](https://github.com/linyinfeng/myml/workflows/Build/badge.svg)](https://github.com/linyinfeng/myml/actions?query=workflow:Build+branch:master)
[![Specification](https://github.com/linyinfeng/myml/workflows/Specification/badge.svg)](https://github.com/linyinfeng/myml/actions?query=workflow:Specification+branch:master)

My toy programming language (working in progress).

## Build and Run

```bash
cabal v2-run mymli
```

## Usage

Use command `:help` in mymli for help information.

## Example

Multi-line terms and bindings can be inputted by `:input` command.

```text
diverge = λ y . (λ x . x x) (λ x . x x)
z = λ f . (λ x . f (λ v . x x v)) (λ x . f (λ v . x x v))

id = λ x . x
const = λ x . λ _ . x

0 = `zro unit
suc = λ n . `suc n
1 = suc 0
2 = suc 1
3 = suc 2

prd = [ `suc n-1 -> n-1, `zro u -> `zro u ]
isZro = [ `suc _ -> false, `zro _ -> true ]

g = λ self . λ n . λ m .
  if isZro n
  then (if isZro m then true else false)
  else (if isZro m then false else self (prd n) (prd m))
eq = z g

eq 0 0
eq 1 0
eq 0 1
eq 1 1
eq 3 3
eq 3 1

setCounterClass =
  λ r . λ self . λ _ .
    { get = λ _ . ! (r.x)
    , set = λ i . r.x := i
    , inc = λ _ . (self unit).set (suc ((self unit).get unit))
    }
setCounterDefaultRep = { x = ref 0 }
createSetCounter =
  λ _ . z (setCounterClass setCounterDefaultRep) unit

c = createSetCounter unit
c.inc unit; c.inc unit; c.get unit
```
