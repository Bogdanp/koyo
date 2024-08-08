import process from "node:process";
import * as esbuild from "esbuild";
import { copy } from "esbuild-plugin-copy";
import { sassPlugin } from "esbuild-sass-plugin";

const production = process.env.NODE_ENV === "production";

const ctx = await esbuild.context({
  entryPoints: ["resources/js/app.ts"],
  outdir: "static",
  target: ["es2020"],
  bundle: true,
  minify: production,
  sourcemap: production,
  plugins: [
    sassPlugin(),
    copy({
      assets: {
        from: "./resources/img/*",
        to: "./img",
      },
    }),
  ],
});

if (production) {
  await ctx.rebuild();
  await ctx.dispose();
} else {
  await ctx.watch();
}
