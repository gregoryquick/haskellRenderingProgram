mydir="$(dirname "$BASH_SOURCE")"
cd $mydir
docker build -t gregoryquick/renderingprogram:v0.1.0 .
ip=$(ifconfig en0 | grep inet | awk '$1=="inet" {print $2}')
xhost + $ip
docker run -t -i \
 -e DISPLAY=$ip:0 \
 -v /tmp/.X11-unix:/tmp/.X11-unix \
 -v $mydir/data:/data \
 gregoryquick/renderingprogram:v0.1.0