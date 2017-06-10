# js99-mode

JavaScript mode aiming to support all EcmaScript extensions by piggybacking off [babylon](https://github.com/babel/babylon) (or, maybe, one of many other JS parsers).

**It's currenly at an early stage of development, don't expect anything to work.**

## Install

### emacs part

1. clone this repo
2. `cask install`
3. `(require 'js99-mode)` from emacs

### node.js part

js99-mode needs [nodejs-js99-mode](https://github.com/futpib/nodejs-js99-mode) for parsing:

```
yarn global add nodejs-js99-mode
```
or
```
npm install -g nodejs-js99-mode
```

