FROM clojure:openjdk-11-tools-deps

WORKDIR /opt/service

COPY deps.edn .
RUN clj -e "(System/exit 0)"

COPY VERSION .
COPY env/ env
COPY src/ src
CMD ["clojure", "-A:run"]
