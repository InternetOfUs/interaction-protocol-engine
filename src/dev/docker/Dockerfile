# syntax=docker/dockerfile:experimental

FROM swipl:stable

# Install java compiling
RUN --mount=type=cache,id=apt-ubuntu-cache-amd64,target=/var/cache/apt apt-get update &&\
	apt-get install -y openjdk-11-jdk maven &&\
	rm -rf /var/lib/apt/lists/*

RUN echo "PS1='\[\033[01;32m\]WeNet@Interaction-Protocol-Engine-Dev\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\] \$ '" >> /root/.bashrc
RUN echo "alias mvnd=\"mvn -Dmaven.surefire.debug=\\\"-Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=0.0.0.0:5554 -Xnoagent -Djava.compiler=NONE\\\"\"" >> /root/.bashrc

WORKDIR /app

ENV JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64

CMD ["bash"]