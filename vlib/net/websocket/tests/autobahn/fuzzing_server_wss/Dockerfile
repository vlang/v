FROM crossbario/autobahn-testsuite
COPY check_results.py /check_results.py
RUN chmod +x /check_results.py

COPY config /config
RUN chmod +rx /config/server.crt
RUN chmod +rx /config/server.key

EXPOSE 9002 9002