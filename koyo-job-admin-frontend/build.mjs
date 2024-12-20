import * as esbuild from "esbuild";
import { copy } from "esbuild-plugin-copy";
import { sassPlugin } from "esbuild-sass-plugin";
import process from "node:process";

const production = process.env.NODE_ENV === "production";
const ctx = await esbuild.context({
  bundle: true,
  outdir: "dist",
  target: ["es2020"],
  minify: production,
  sourcemap: true,
  define: {
    "window.ENVIRONMENT": JSON.stringify(production ? "production" : "dev"),
  },
  loader: {
    ".woff": "dataurl",
    ".woff2": "dataurl",
  },
  plugins: [
    sassPlugin(),
    copy({
      assets: {
        from: "./img/*",
        to: "./img",
      },
    }),
  ],
  entryPoints: ["src/app.tsx"],
});
if (production) {
  await ctx.rebuild();
  await ctx.dispose();
} else {
  await ctx.watch();
}
