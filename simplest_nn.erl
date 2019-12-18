%Source code was iterated from Gene Sher author of Handbook of Neuroevolution published by Springer Science+Media Books in 2013. See CorticalComputers DXNN project for licensing info.

-module(simplest_nn).
-compile(export_all).

create()->
	Weights = [rand:uniform()-0.5,rand:uniform()-0.5,rand:uniform()-0.5],
	N_PId = spawn(?MODULE,neuron,[Weights,undefined,undefined]),
	S_PId = spawn(?MODULE,sensor,[N_PId]),
	A_PId = spawn(?MODULE,actuator,[N_PId]),
	N_PId ! {init,S_PId,A_PId},
	register(cortex,spawn(?MODULE,cortex,[S_PId,N_PId,A_PId])).
%Genesis cortex spawned, Create function spawns a single cortex, where the weights and the Bias value are generated randomly between -0.0 and 0.5. The third weight is the Bias. The neuron is spawned first, and is then sent the PIds of the Sensor and Actuator that its connected with. Then the Cortex processing element is registered and provided with the PIds of all the processing elements in the NN system.

neuron(Weights,S_PId,A_PId) ->
	receive
		{S_PId,forward, Input} ->
			io:format("****Thinking****~n Input:~p~n with Weights:~p~n",[Input,Weights]),
			Dot_Product = dot(Input,Weights,0),
			Output = [math:tanh(Dot_Product)],
			A_PId ! {self(),forward,Output},
			neuron(Weights,S_PId,A_PId);
		{init,New_SPId,New_APId} ->
			neuron(Weights,New_SPId,New_APId);
		terminate ->
			ok
	end.
%Neuron signal processing, Once the neuron finishes setting it SPId and APId to that of the Sensor and Actuator respectively, it begins waiting for incoming signals. Neuron expects the vector length of TWO as input, and as soon as the input arrives, the neuron processes the signal and passes the output vector to the outgoing APId.

	dot([I|Input],[W|Weights],Acc) ->
		dot(Input,Weights,I*W+Acc);
	dot([],[],Acc)->
		Acc;
	dot([],[Bias],Acc)->
		Acc + Bias.
%The dot function takes a dot product of 2 vectors, it can operate on a weight vector with and without a bias. When there is no bias in the weight list, both the Input and the weight vector are of the same length. When bias is present, then when the Input list empties out, the Weights list still has 1 value remaining, which is Bias.

sensor(N_PId) ->
	receive
		sync ->
			Sensory_Signal = [rand:uniform(),rand:uniform()],
			io:format("****Sensing****:~n Signal from the environment~p~n",[Sensory_Signal]),
			N_PId ! {self(),forward,Sensory_Signal},
			sensor(N_PId);
		terminate ->
			ok
	end.

%Sensor signal processing, The Sensor function waits for to be triggered by the Cortex element, then produces a random vector of length 2, which it passes to the connected neuron. The sensory signal would not be a random vector in a proper system, but instead be produced by a function associated with the sensor. The sensor could, for example, read and vector-encode a signal coming from a GPS attached to a robot.

actuator(N_PId) -> 
	receive
		{N_PId,forward,Control_Signal}->
			pts(Control_Signal),
			actuator(N_PId);
		terminate ->
			ok
	end.
	
	pts(Control_Signal)->
		io:format("****Acting****:~n Using :~p to act on environment.~n",[Control_Signal]).
%Actuator signal processing and propagation, The Actuator function waits for a control signal coming from a Neuron. As soon as the signal arrives, the actuator executes its function, pts/1, which prints the value to the screen.

cortex(Sensor_PId,Neuron_PId,Actuator_PId)->
	receive
		sense_think_act ->
			Sensor_PId ! sync,
			cortex(Sensor_PId,Neuron_PId,Actuator_PId);
		terminate ->
			Sensor_PId ! terminate,
			Neuron_PId ! terminate,
			Actuator_PId ! terminate,
			ok
	end.
%Cortex triggers sensor upon command, The Cortex function triggers the sensor to action when commanded by the user. This process can also terminate the entire system when requested since it has all of the NN systems element PIds.