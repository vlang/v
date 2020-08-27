FROM thevlang/vlang:buster-dev

# ARG WORKSPACE_ROOT

# WORKDIR ${WORKSPACE_ROOT}
COPY ./ /src/
# COPY tests/autobahn/ws_test/run.sh /run.sh
# RUN chmod +x /run.sh
RUN v -autofree /src/tests/autobahn/autobahn_server.v
RUN chmod +x /src/tests/autobahn/autobahn_server
ENTRYPOINT [ "/src/tests/autobahn/autobahn_server" ]
