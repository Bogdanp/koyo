import * as esbuild from "esbuild";
import { copy } from "esbuild-plugin-copy";
import { sassPlugin } from "esbuild-sass-plugin";
import process from "node:process";

const production = process.env.NODE_ENV === "production";

const ctx = await esbuild.context({
  entryPoints: ["frontend/app.tsx"],
  outdir: "static",
  target: ["es2020"],
  bundle: true,
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
        from: "./frontend/img/*",
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
