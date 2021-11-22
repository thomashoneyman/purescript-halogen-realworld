const Main = process.env.NODE_ENV !== "production" ? require("./output/Main") : require("./dce-output/Main");
Main.main();
