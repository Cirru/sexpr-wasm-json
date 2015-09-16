
var
  gulp $ require :gulp
  script $ require :gulp-cirru-script

gulp.task :script $ \ ()
  ... gulp
    src :src/*.cirru
    pipe $ script
    pipe $ gulp.dest :lib/

