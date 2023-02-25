const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');
const webpack = require('webpack'); // uncomment to enable DefinePlugin

module.exports = env => {

  const { NPC_ENV } = env;

  const NODE_CONSTANTS = require(`./constants/node/${NPC_ENV}.js`);
  NODE_CONSTANTS.NPC_ENV = NPC_ENV;

  return {
    mode: 'development', // put to 'production' to create less code
    devtool: 'inline-source-map', // put false to remove source maps
    entry: './src/index.js',
    watch: true,
    resolve: {
      extensions: ['.js'],
      alias: {
        '@src': path.resolve(__dirname, './src'),
      },
    },
    output: {
      filename: 'bundle.js',
      path: path.resolve(__dirname, 'dist'),
    },
    module: {
      rules: [
        {
          test: /\.css$/i,
          use: ['style-loader', 'css-loader'],
        },
        {
          test: /\.(png|svg|jpg|jpeg|gif)$/i,
          type: 'asset/resource',
        },
        {
          test: /\.(js)$/,
          exclude: /node_modules/,
          use: ['babel-loader'],
        },
      ],
    },
    plugins: [
      new HtmlWebpackPlugin({
        template: path.resolve(__dirname, './public', 'index.html'),
        favicon: path.resolve(__dirname, './public/favicon.ico'),
        inject: true, // set to false if <script src="bundle.js"></script> is present in index.html
      }),
      new CopyWebpackPlugin([{
        from: path.resolve(__dirname, `./constants/runtime/${NPC_ENV}.js`),
        to: path.resolve(__dirname, './dist/configurations.js'),
      }]),
      new webpack.DefinePlugin({
        ENV2: JSON.stringify(NODE_CONSTANTS),
      }),
    ],
  };
};
