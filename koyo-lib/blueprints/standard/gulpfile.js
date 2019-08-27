const { watch, src, dest, parallel, series } = require("gulp");
const sass = require("gulp-sass");
const livereload = require("gulp-livereload");
const concat = require("gulp-concat");
const del = require('del');
const sourcemaps = require('gulp-sourcemaps');
const browserSync = require("browser-sync").create();


function copy() {
  return src("input/*.js")
    .pipe(dest("output/"));
}

function compileSass() {
  return src("resources/css/*.scss")
    .pipe(sourcemaps.init())
    .pipe(sass())
    .pipe(sourcemaps.write())
    .pipe(dest("static/css"))
    .pipe(livereload());
}

function copyImages() {
  return src("resources/img/*").pipe(dest("static/img/"));
}

function copyVendor() {
  return src("resources/vendor/*").pipe(dest("static/vendor/"));
}

function assembleJS() {
  return src("resources/js/*").pipe(concat("app.js")).pipe(dest("static/js"));
}


async function clean() {
  del(["static"]);
}

const build = parallel(compileSass, copyImages, copyVendor, assembleJS);

function defaultTask(cb) {
  // place code for your default task here
  cb();
}

function reload(done) {
  browserSync.reload();
  done();
}

function serve(cb) {
  browserSync.init({
    proxy: "localhost:8000",
    serveStatic: [{
      route: '/static',
      dir: './static'
    }]
  });
  watch("resources/**", series(build, reload));
}


exports.copy = copy;
exports.clean = clean;
exports.compileSass = compileSass;
exports.copyImages = copyImages;
exports.copyVendor = copyVendor;
exports.assembleJS = assembleJS;
exports.build = build;

exports.serve = series(build, serve);

exports.default = defaultTask;
