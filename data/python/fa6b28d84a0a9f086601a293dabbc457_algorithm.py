import json
import logging
from flask import request
from flask.ext import restful
import model
import api


class AlgorithmList(restful.Resource):

    def get(self):
        l = model.CONFIG["algorithm"]["available"]
        return l, 200, api.CORS_HEADER

    def options(self):
        return ({'Allow': 'GET'}, 200,
                {'Access-Control-Allow-Origin': '*',
                 'Access-Control-Allow-Methods': 'GET'})


class SelectedAlgorithm(restful.Resource):

    def get(self):
        json_data = {}
        logging.debug("GET Algorithm response body: %s", str(json_data))
        return json_data, 200, api.CORS_HEADER

    def get(self):
        d = {"selected": model.CONFIG["algorithm"]["default"]}
        return d, 200, api.CORS_HEADER

    def put(self):
        try:
            json_data = request.get_json(force=True)
        except:
            raise JsonRequestParsingError("Request parsing error")
        logging.debug("PUT request body: %s" % str(json_data))
        if "selected" not in json_data:
            raise RequestDataError
        if (json_data["selected"] not in model.CONFIG["algorithm"]["available"]):
            raise RequestDataError
        # request is valid, change model
        model.CONFIG["algorithm"]["default"] = json_data["selected"]
        # send update signal
        api.zmq_send(json.dumps({"action": "switch_algorithm", "algorithm": json_data["selected"]}))
        return None, 204, api.CORS_HEADER

    def options(self):
        return ({'Allow': 'POST,PUT,GET,DELETE'}, 200,
                {'Access-Control-Allow-Origin': '*',
                 'Access-Control-Allow-Methods': 'POST,PUT,GET,DELETE',
                 'Access-Control-Allow-Headers': 'Content-Type'})
