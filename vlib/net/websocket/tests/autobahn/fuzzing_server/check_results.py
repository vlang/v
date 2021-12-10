import json

nr_of_client_errs = 0
nr_of_client_tests = 0

nr_of_server_errs = 0
nr_of_server_tests = 0

with open("/reports/clients/index.json") as f:
    data = json.load(f)

    for i in data["v-client"]:
        # Count errors
        if (
            data["v-client"][i]["behavior"] == "FAILED"
            or data["v-client"][i]["behaviorClose"] == "FAILED"
        ):
            nr_of_client_errs = nr_of_client_errs + 1

        nr_of_client_tests = nr_of_client_tests + 1

with open("/reports/servers/index.json") as f:
    data = json.load(f)

    for i in data["AutobahnServer"]:
        if (
            data["AutobahnServer"][i]["behavior"] == "FAILED"
            or data["AutobahnServer"][i]["behaviorClose"] == "FAILED"
        ):
            nr_of_server_errs = nr_of_server_errs + 1

        nr_of_server_tests = nr_of_server_tests + 1

if nr_of_client_errs > 0 or nr_of_server_errs > 0:
    print(
        "FAILED AUTOBAHN TESTS, CLIENT ERRORS {0}(of {1}), SERVER ERRORS {2}(of {3})".format(
            nr_of_client_errs, nr_of_client_tests, nr_of_server_errs, nr_of_server_tests
        )
    )
    exit(1)

print(
    "TEST SUCCESS!, CLIENT TESTS({0}), SERVER TESTS ({1})".format(
        nr_of_client_tests, nr_of_server_tests
    )
)
