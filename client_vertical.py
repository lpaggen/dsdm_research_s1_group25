import tensorflow as tf
import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
import flwr as fl

def load_data(client_id):
    df = pd.read_csv(r"") # path to dataset on your machine

    X, y = df.drop("Outcome", axis=1).values, df["Outcome"].values # split X Y 

    # this is to be removed if you use real data from actual different providers
    if client_id == 1: # remember constant client ID defined in __main__
        X = X[:, :4]  # in contrast to horizontal FL, here each client has a subset of the features of the dataset
    else:
        X = X[:, 4:]

    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42) # test train 

    scaler = StandardScaler() # need standardizing for NN
    X_train = scaler.fit_transform(X_train)
    X_test = scaler.transform(X_test)

    return X_train, X_test, y_train, y_test

def create_model(input_shape):
    model = tf.keras.Sequential([
        tf.keras.layers.Input(shape=input_shape),
        tf.keras.layers.Dense(256, activation="relu"),
        tf.keras.layers.Dense(128, activation="relu"),
        tf.keras.layers.Dense(128, activation="relu"),
        tf.keras.layers.Dense(128, activation="relu"),
        tf.keras.layers.Dense(64, activation="relu"),
        tf.keras.layers.Dense(1, activation="sigmoid")
    ])
    model.compile(optimizer="adam", loss="binary_crossentropy", metrics=["accuracy"])
    return model

class DiabetesClient(fl.client.NumPyClient): # deprecated, look at flower documentation for more information
    def __init__(self, model, X_train, X_test, y_train, y_test):
        self.model = model
        self.X_train = X_train
        self.X_test = X_test
        self.y_train = y_train
        self.y_test = y_test

    def get_parameters(self):
        print("Sending parameters to the server")
        print(self.model.get_weights())
        return self.model.get_weights()

    def fit(self, parameters, config):
        secure_agg_enabled = config.get("secure_aggregation", True) # unsure if this works wthout altering the server code to support it
        print(f"secAgg: {secure_agg_enabled}")
        
        self.model.set_weights(parameters)
        self.model.fit(self.X_train, self.y_train, epochs=5, batch_size=32, verbose=0)
        updated_parameters = self.model.get_weights()
        return updated_parameters, len(self.X_train), {}

    def evaluate(self, parameters):
        print("Evaluating model with received parameters")
        self.model.set_weights(parameters)
        loss, accuracy = self.model.evaluate(self.X_test, self.y_test, verbose=0)
        print(f"Evaluation results - Loss: {loss}, Accuracy: {accuracy}")
        return loss, len(self.X_test), {"accuracy": accuracy}

if __name__ == "__main__":
    CLIENT_ID = 1

    X_train, X_test, y_train, y_test = load_data(CLIENT_ID)

    model = create_model(X_train.shape[1:])

    client = DiabetesClient(model, X_train, X_test, y_train, y_test)
    fl.client.start_numpy_client(server_address="ip:port", client=client)
