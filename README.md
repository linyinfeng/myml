# myml

[![Build](https://github.com/linyinfeng/myml/workflows/Build/badge.svg)](https://github.com/linyinfeng/myml/actions?query=workflow:Build+branch:master)
[![Specification](https://github.com/linyinfeng/myml/workflows/Specification/badge.svg)](https://github.com/linyinfeng/myml/actions?query=workflow:Specification+branch:master)

My toy programming language (working in progress).

## Build and Run

```bash
cabal v2-run mymli
```

## Installation

```bash
git clone https://github.com/linyinfeng/myml.git
cd myml
cabal v2-install mymli
```

## Usage

Pass command line option `--help` to mymli for help information of mymli CLI.

Use command `:help` in mymli for help information.

## Example

Multi-line terms and bindings can be inputted by `:input` command.

```text
diverge _ = (λ x . x x) (λ x . x x) ;;
z f = (λ x . f (λ v . x x v)) (λ x . f (λ v . x x v)) ;;

id x = x ;;
const x _ = x ;;

g = λ eq . λ n . λ m .
  if isZero n
  then (if isZero m then true else false)
  else (if isZero m then false else eq (pred n) (pred m)) ;;
natEq = z g ;;

Counter =
  class
    with r
    { get _ = ! r.x
    , inc _ = r.x := succ (! r.x) } ;;
BackupCounter =
  class
    with r
    inherit Counter as super
    super with
      { backup _ =  r.b := ! r.x
      , restore _ = r.x := ! r.b } ;;
EvenCounter =
  class
    with r
    inherit Counter as super
    super with
      { inc _ = super.inc (); super.inc () } ;;
c = new BackupCounter { x = ref 0, b = ref 0 } ;;
```
