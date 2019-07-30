var path = require("path");

function resolve(filePath) {
  return path.join(__dirname, filePath)
}

module.exports = {
  entry: [resolve("src/Electron/Renderer/Renderer.fsproj"), resolve("src/Electron/Renderer/scss/main.scss")],
  output: {
    filename: "renderer.js"
  },
  module: {
    rules: [
      {
        test: /\.fs(x|proj)?$/,
        use: {
          loader: "fable-loader"
        }
      }
    ]
  }
}
