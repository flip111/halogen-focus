var Main = require('./output/Main');

function main () {
    /*
    Here we could add variables such as

    var baseUrl = process.env.BASE_URL;

    Parcel will replace `process.env.BASE_URL`
    with the string contents of the BASE_URL environment
    variable at bundle/build time.
    A .env file can also be used to override shell variables
    for more information, see https://en.parceljs.org/env.html

    These variables can be supplied to the Main.main function.
    However, you will need to change the type to accept variables, by default it is an Effect.
    You will probably want to make it a function from String -> Effect ()
  */

    Main.main();
}

// HMR setup. For more info see: https://parceljs.org/hmr.html
if (module.hot) {
  module.hot.accept(function () {
    if (Main && typeof Main.unsubscribe === "function") {
      Main.unsubscribe();
    }

    document.body.innerHTML = '';

    console.log('Reloaded, running main again');
    main();
  });
}

main();