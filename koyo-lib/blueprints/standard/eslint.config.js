import js from "@eslint/js";

export default [
  js.configs.recommended,
  {
    files: [
      "resources/js/**/*.js",
    ],
    rules: {
      indent: ["error", 2],
      "linebreak-style": ["error", "unix"],
      quotes: ["error", "double"],
      semi: ["error", "always"],
    },
  },
];
