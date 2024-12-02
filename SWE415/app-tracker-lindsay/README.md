## Building the module

```shell
npm run clean && npm run build
```

## Pulling the window list
```shell
node print_windows.js
```

## Including this as a module

### install/update the module
```shell
yarn add https://github.com/gozowy/app-tracker -W
```

### using the module in typescript
```ts
import { fetchDesktopWindows } from 'app-tracker';

fetchDesktopWindows().then(desktopWindows => {
    desktopWindows.forEach(desktopWindow => {
        console.log(desktopWindow);
    });
});
```