import flwr as fl
import tensorflow as tf
import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler

def load_data(client_id):
    df = pd.read_csv(r"") # replace with path to any dataset, i have r prefix because of some problem i encountered
    X, y = df.drop("Outcome", axis=1).values, df["Outcome"].values # sep into X Y 

    # split the dataset according to client id, need to scale with number of clients (but in real life, no splitting needed at this stage)
    # after all you would use real data from diff sources and splitting would not make sense, so this has to be removed
    if client_id == 1:
        X, y = X[:len(X)//2], y[:len(y)//2]
    else:  # client_id == 2
        X, y = X[len(X)//2:], y[len(y)//2:]

    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42) # using sklearn to split test-train, could just use indexing too

    scaler = StandardScaler() # for the NN especially, they tend to require standardization
    X_train = scaler.fit_transform(X_train)
    X_test = scaler.transform(X_test)

    return X_train, X_test, y_train, y_test

def create_model(input_shape): # here we can select any model from tensorflow, could use pytorch too, tensorflow tends to be easier
    model = tf.keras.Sequential([
        tf.keras.layers.Input(shape=input_shape),  # set to number of X variables in dataset 
        tf.keras.layers.Dense(16, activation="relu"),
        tf.keras.layers.Dense(8, activation="relu"),
        tf.keras.layers.Dense(1, activation="sigmoid") # sigmoid for binary output
    ])
    model.compile(optimizer="adam", loss="binary_crossentropy", metrics=["accuracy"])
    return model

class DiabetesClient(fl.client.NumPyClient): # the numpyClient is deprecated, check the docs for more info on what is best practice here
    def __init__(self, model, X_train, X_test, y_train, y_test):
        self.model = model
        self.X_train = X_train
        self.X_test = X_test
        self.y_train = y_train
        self.y_test = y_test

    def get_parameters(self):
        print("sharing params to server")
        print(self.model.get_weights())
        return self.model.get_weights()

    def fit(self, parameters, config):
        secure_agg_enabled = config.get("secure_aggregation", True) # i do not think this actually works without adaptation to the server's code
        print(f"secAgg: {secure_agg_enabled}") # more research should be done at this stage
        
        self.model.set_weights(parameters)
        self.model.fit(self.X_train, self.y_train, epochs=5, batch_size=32, verbose=0)
        updated_parameters = self.model.get_weights()
        return updated_parameters, len(self.X_train), {}

    def evaluate(self, parameters):
        print("evaluating model")
        self.model.set_weights(parameters)
        loss, accuracy = self.model.evaluate(self.X_test, self.y_test, verbose=0)
        print(f"-> loss: {loss}, accuracy: {accuracy}")
        return loss, len(self.X_test), {"accuracy": accuracy}

if __name__ == "__main__":
    CLIENT_ID = 1 # remove this when you run without splitting a dataset

    X_train, X_test, y_train, y_test = load_data(CLIENT_ID)

    model = create_model(X_train.shape[1:])

    client = DiabetesClient(model, X_train, X_test, y_train, y_test)
    fl.client.start_numpy_client(server_address="ip:port", client=client) # you should replace this with own server ID, flower using 8080 default port
