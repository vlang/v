# Use this as docker builder with https://github.com/nektos/act
# build with: docker build tests/autobahn/. -t myimage
# use in act: act -P ubuntu-latest=myimage

FROM node:12.6-buster-slim

COPY config/fuzzingserver.json /config/fuzzingserver.json
RUN chmod +775 /config/fuzzingserver.json
RUN apt update && apt install -y docker
