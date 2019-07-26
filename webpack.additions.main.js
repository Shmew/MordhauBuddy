var path = require("path");

function resolve(filePath) {
  return path.join(__dirname, filePath)
}

module.exports = {
    entry: resolve("src/Electron/Main/Main.fsproj"),
    resolve: {
        alias: {
            'react': path.resolve('node_modules/react'),
            'react-dom': path.resolve('node_modules/react-dom'),
            'react-spring': path.resolve('node_modules/react-spring')
        }
    },
  externals: {
    bufferutil: 'commonjs bufferutil',
    'utf-8-validate': 'commonjs utf-8-validate',
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
