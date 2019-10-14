mydir="$(dirname "$BASH_SOURCE")"
cd $mydir
docker build -t gregoryquick/project0:v0.1.2 .
ip=$(ifconfig en0 | grep inet | awk '$1=="inet" {print $2}')
xhost + $ip
docker run -t -i -e DISPLAY=$ip:0 -v /tmp/.X11-unix:/tmp/.X11-unix gregoryquick/project0:v0.1.2