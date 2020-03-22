# myml

[![Build](https://github.com/linyinfeng/myml/workflows/Build/badge.svg)](https://github.com/linyinfeng/myml/actions?query=workflow:Build+branch:master)

My toy programming language (working in progress).

Based on Hindley-Milner type system, with

* Equi-recursive types
* Extensible records and extensible variants based on row polymorphism
* Reference(Optional)
* Class as derived form

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
-- module system is unfinished, only file can be imported now
import "stdlib/integer.myml" ;;

-- single line comment
{-# multi-line comment can be {-# nested #-} #-}

-- `<f> <x> = <t>` is a derived form of `<f> = λ <x> . <t>`
-- use `;;` to end input
id x = x ;;
const x _ = x ;;

-- equi-recursive types enable the z combinator to be typed
diverge _ = (λ x . x x) (λ x . x x) ;;
z f = (λ x . f (λ v . x x v)) (λ x . f (λ v . x x v)) ;;

r = { } ;;
rExtended = r extend { l = id } ;;

Counter r =
  class
    { get _ = ! r.x
    , inc _ = r.x := integerPlus 1 (! r.x) } ;;
BackupCounter r =
  class
    inherit Counter r as super
    let b = ref (! r.x) in
      super extend
        { backup _ =  b := ! r.x
        , restore _ = r.x := ! b } ;;
EvenCounter r =
  class
    inherit Counter r as super
    super update
      { inc _ = super.inc (); super.inc () } ;;
EvenBackupCounter r =
  class
    inherit EvenCounter r as super
    inherit BackupCounter r as superBackup
    super extend
      { backup = superBackup.backup
      , restore = superBackup.restore } ;;
c = new (EvenBackupCounter { x = ref 0 }) ;;
```
