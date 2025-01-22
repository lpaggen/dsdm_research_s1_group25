import flwr as fl
import logging

# check the flower documentation to adapt the code to your needs
# https://flower.ai/docs/examples/

logging.basicConfig(level=logging.INFO) # enable this for the INFO (logs) in command line

class FedAvgWithSecAgg(fl.server.strategy.FedAvg): # can use other than fedAvg here, check supported arguments on Flower docs
    "here it is possible to implement secure aggregation, it is explained on the original Google paper and on Flower docs, I could not get it running however"
    # the flower docs have such an example here: https://flower.ai/docs/examples/flower-secure-aggregation.html
    def aggregate_fit(self, rnd, results, failures):
        logging.info("Round %d", rnd)
        aggregated_parameters = super().aggregate_fit(rnd, results, failures)
        return aggregated_parameters

def get_strategy():
    def aggregate_evaluate(metrics): # it's possible to add more metrics here 
        try:
            accuracies = [m["accuracy"] for i, m in metrics if "accuracy" in m]
            return {"accuracy": sum(accuracies) / len(accuracies)} if accuracies else {}
        except Exception as e:
            logging.error(f"Error during metrics aggregation: {e}")
            return {} # in case of error should return empty map

    return FedAvgWithSecAgg( # inherits from the FedAvg, as defined above in the class itself
        fraction_fit=1.0,  # 1 means we use all clients, for larger setups, this can be changed, it's a fraction of the total clients 
        fraction_evaluate=1.0,  # again, 1 means we use all clients data to evaluate
        min_fit_clients=2,  # our number of clients was 2, but this could be adjusted to your needs
        min_evaluate_clients=2, # these next 2 fields determine how many clients at minimum are needed to proceed
        min_available_clients=2,
        evaluate_metrics_aggregation_fn=aggregate_evaluate,
    )

if __name__ == "__main__":
    num_rounds = 50  # can set to anything
    logging.info("starting Flower server with %d rounds", num_rounds)

    fl.server.start_server( # this actually starts the server
        server_address="ip:port",  # use your own ip address and port, by default Flower uses 8080
        config=fl.server.ServerConfig(num_rounds=num_rounds),
        strategy=get_strategy(),
    )
