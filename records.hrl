%Source code was iterated from Gene Sher author of Handbook of Neuroevolution published by Springer Science+Media Books in 2013. See CorticalComputers DXNN project for licensing info.

-record(sensor, {id, cx_id, name, vl, fanout_ids}).
-record(actuator, {id, cx_id, name, vl, fanin_ids}).
-record(neuron, {id, cx_id, af, input_idps, output_ids}).
-record(cortex, {id, sensor_ids, actuator, output_ids}).
%Creating record directives to represent each element we'll use.