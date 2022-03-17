# syntax=docker/dockerfile:experimental

FROM ubuntu:latest

ENV DEBIAN_FRONTEND=noninteractive \
LANG=en_US.UTF-8 \
LANGUAGE=en_US.UTF-8 \
LC_ALL=en_US.UTF-8 \
TZ=:/etc/UTC
RUN --mount=type=cache,id=apt-ubuntu-cache-amd64,target=/var/cache/apt apt-get -y update
RUN --mount=type=cache,id=apt-ubuntu-cache-amd64,target=/var/cache/apt apt-get -y install locales
RUN locale-gen en_US.UTF-8
RUN dpkg-reconfigure locales
RUN --mount=type=cache,id=apt-ubuntu-cache-amd64,target=/var/cache/apt apt-get -y install aptitude
RUN --mount=type=cache,id=apt-ubuntu-cache-amd64,target=/var/cache/apt aptitude -y update
RUN --mount=type=cache,id=apt-ubuntu-cache-amd64,target=/var/cache/apt aptitude -y full-upgrade

# Install java compiling
RUN --mount=type=cache,id=apt-ubuntu-cache-amd64,target=/var/cache/apt aptitude -y install openjdk-11-jdk
RUN --mount=type=cache,id=apt-ubuntu-cache-amd64,target=/var/cache/apt aptitude -y install maven

# Install swi prolog
RUN --mount=type=cache,id=apt-ubuntu-cache-amd64,target=/var/cache/apt aptitude -y install software-properties-common
RUN apt-add-repository ppa:swi-prolog/stable
RUN --mount=type=cache,id=apt-ubuntu-cache-amd64,target=/var/cache/apt aptitude -y update
RUN --mount=type=cache,id=apt-ubuntu-cache-amd64,target=/var/cache/apt aptitude -y full-upgrade
RUN --mount=type=cache,id=apt-ubuntu-cache-amd64,target=/var/cache/apt aptitude -y install swi-prolog

# Install utilities
RUN --mount=type=cache,id=apt-ubuntu-cache-amd64,target=/var/cache/apt aptitude -y install less

RUN echo "PS1='\[\033[01;32m\]WeNet@Interaction-Protocol-Engine-Dev\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\] \$ '" >> /root/.bashrc
RUN echo "alias mvnd=\"mvn -Dmaven.surefire.debug=\\\"-Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=0.0.0.0:5554 -Xnoagent -Djava.compiler=NONE\\\"\"" >> /root/.bashrc

WORKDIR /app

ENV JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64

CMD ["bash"]