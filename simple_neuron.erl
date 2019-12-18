%Source code was iterated from Gene Sher author of Handbook of Neuroevolution published by Springer Science+Media Books in 2013. See CorticalComputers DXNN project for licensing info.

-module(simple_neuron).
-compile(export_all).

create()->
	Weights = [rand:uniform()-0.5,rand:uniform()-0.5,rand:uniform()-0.5],
	register(neuron, spawn(?MODULE,loop,[Weights])).
%Genesis neuron spawned, Create function spawns a single neuron, where the weights and the bias values are generated randomly between -0.0 and 0.5.

loop(Weights) ->
	receive
		{From, Input} ->
			io:format("****Processing****~n Input:~p~n Using Weights:~p~n",[Input,Weights]),
			Dot_Product = dot(Input,Weights,0),
			Output = [math:tanh(Dot_Product)],
			From ! {result,Output},
			loop(Weights)
	end.
%Function declaration, The spawned neuron process accepts an input vector, prints it and the weight vector to the screen, calculates the output, then sends the output to the contacting process. The output is a vector length of 1.

	dot([I|Input],[W|Weights],Acc) ->
		dot(Input,Weights,I*W+Acc);
	dot([],[Bias],Acc)->
		Acc + Bias.
%List comprehension, Completed list comprehension required since the used dot product function assumes bias is incorporated as the last value on the weight list. Perhaps this is also a recursive list tranformation, pattern number 0.

sense(Signal)->
	case is_list(Signal) and (length(Signal) == 2) of
		true->
			neuron ! {self(),Signal},
			receive
				{result,Output}->
				io:format("Ouput: ~p~n",[Output])
			end;
		false->
			io:format("The Signal must be a list of length 2~n")
	end.
%Case statement, Initiate sense function to contact the neuron and send it an input vector, sense function also ensures the signal we are sending is a vector length of 2.