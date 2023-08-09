import numpy as np


from tensorflow import keras
from tensorflow.keras import layers
from tensorflow.keras import regularizers

def exp_transform(vec):
	nvals = len(vec)
	vec_out = np.zeros(nvals)
	for nv in range(nvals):
		vec_out[nv] = 1.0/(1.0 + np.exp(-1.0*vec[nv]))
	
	return vec_out

# This is a pure numpy version of the function
def predict(input_vec, weights, biases):
	layer1 = np.matmul(weights[0],input_vec)
	layer1 += biases[0]
	layer1 = exp_transform(layer1)
	layer2 = np.matmul(weights[1],layer1)
	layer2 += biases[1]
	layer2 = exp_transform(layer2)
	layer3 = np.matmul(weights[2],layer2)
	layer3 += biases[2]
	return layer3

def create_fnet_model(input_dim):
	# Define the model architecture
	model = keras.Sequential([
		keras.layers.Dense(8, activation='sigmoid', input_dim=input_dim),
		keras.layers.Dense(2, activation='sigmoid'),
		keras.layers.Dense(1)
	])
	
	# Compile the model with an appropriate loss function and optimizer
	model.compile(loss='mean_squared_error', optimizer='adam')
	return model

def set_weights(the_model, weights, biases):
	#input_array is from fortran, need to adjust it for numpy
	the_model.layers[0].set_weights([np.transpose(weights[0]), biases[0]])
	the_model.layers[1].set_weights([np.transpose(weights[1]), biases[1]])
	the_model.layers[2].set_weights([(weights[2].reshape(-1, 1)), biases[2]])  # seems to need [2,1] and not [2,]

# Just an example of how we can pull the weights and biases out of a tensorflow model.
def print_tensorflow_details(model):
	np.set_printoptions(threshold=np.inf)
	for layer in model.layers:
		weights, biases = layer.get_weights()
		print(f"Layer: {layer.name}")
		print(f"Weights shape: {weights.shape}")
		print(f"Weights:\n{weights}")
		print(f"Biases shape: {biases.shape}")
		print(f"Biases:\n{biases}")
		print("===================================")

def tf_predict(input_vec, weights, biases):
	model = create_fnet_model(126)
	set_weights(model, weights, biases)
	# to do a single prediction we input to be in a 1 element array
	# we get back an array per prediction so pull out via [0,0]
	return model.predict(np.array([input_vec]))[0,0]
	

