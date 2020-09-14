FROM crossbario/autobahn-testsuite
COPY check_results.py /check_results.py
RUN chmod +x /check_results.py

COPY config/fuzzingserver.json /config/fuzzingserver.json
COPY config/fuzzingclient.json /config/fuzzingclient.json
