// this code was generated using the rkwarddev package.
//perhaps don't make changes here, but in the rkwarddev script instead!



function preprocess(){
	// add requirements etc. here
	echo("require(cocor)\n");

	echo("require(cocor)\n");
}

function calculate(){
	// read in variables from dialog
	var groups = getValue("groups");
	var correlations = getValue("correlations");
	var dataInput = getValue("data_input");
	var manualIndepGroupsR1 = getValue("manual_indep_groups_r1");
	var manualIndepGroupsR2 = getValue("manual_indep_groups_r2");
	var manualIndepGroupsN1 = getValue("manual_indep_groups_n1");
	var manualIndepGroupsN2 = getValue("manual_indep_groups_n2");
	var manualDepGroupsOverlapRJk = getValue("manual_dep_groups_overlap_r_jk");
	var manualDepGroupsOverlapRJh = getValue("manual_dep_groups_overlap_r_jh");
	var manualDepGroupsOverlapRKh = getValue("manual_dep_groups_overlap_r_kh");
	var manualDepGroupsOverlapN = getValue("manual_dep_groups_overlap_n");
	var manualDepGroupsNonoverlapRJk = getValue("manual_dep_groups_nonoverlap_r_jk");
	var manualDepGroupsNonoverlapRHm = getValue("manual_dep_groups_nonoverlap_r_hm");
	var manualDepGroupsNonoverlapRJh = getValue("manual_dep_groups_nonoverlap_r_jh");
	var manualDepGroupsNonoverlapRJm = getValue("manual_dep_groups_nonoverlap_r_jm");
	var manualDepGroupsNonoverlapRKh = getValue("manual_dep_groups_nonoverlap_r_kh");
	var manualDepGroupsNonoverlapRKm = getValue("manual_dep_groups_nonoverlap_r_km");
	var manualDepGroupsNonoverlapN = getValue("manual_dep_groups_nonoverlap_n");
	var variableIndepGroupsR1 = getValue("variable_indep_groups_r1");
	var variableIndepGroupsR2 = getValue("variable_indep_groups_r2");
	var variableIndepGroupsN1 = getValue("variable_indep_groups_n1");
	var variableIndepGroupsN2 = getValue("variable_indep_groups_n2");
	var variableDepGroupsOverlapRJk = getValue("variable_dep_groups_overlap_r_jk");
	var variableDepGroupsOverlapRJh = getValue("variable_dep_groups_overlap_r_jh");
	var variableDepGroupsOverlapRKh = getValue("variable_dep_groups_overlap_r_kh");
	var variableDepGroupsOverlapN = getValue("variable_dep_groups_overlap_n");
	var variableDepGroupsNonoverlapRJk = getValue("variable_dep_groups_nonoverlap_r_jk");
	var variableDepGroupsNonoverlapRHm = getValue("variable_dep_groups_nonoverlap_r_hm");
	var variableDepGroupsNonoverlapRJh = getValue("variable_dep_groups_nonoverlap_r_jh");
	var variableDepGroupsNonoverlapRJm = getValue("variable_dep_groups_nonoverlap_r_jm");
	var variableDepGroupsNonoverlapRKh = getValue("variable_dep_groups_nonoverlap_r_kh");
	var variableDepGroupsNonoverlapRKm = getValue("variable_dep_groups_nonoverlap_r_km");
	var variableDepGroupsNonoverlapN = getValue("variable_dep_groups_nonoverlap_n");
	var testHypothesisIndepGroups = getValue("test_hypothesis_indep_groups");
	var testHypothesisDepGroupsOverlap = getValue("test_hypothesis_dep_groups_overlap");
	var testHypothesisDepGroupsNonoverlap = getValue("test_hypothesis_dep_groups_nonoverlap");
	var alpha = getValue("alpha");

	// the R code to be evaluated
	if(dataInput == 'manual' && groups == 'indep') {
		echo("result <- cocor.indep.groups(r1=" + manualIndepGroupsR1 + ", n1=" + manualIndepGroupsN1 + ", r2=" + manualIndepGroupsR2 + ", n2=" + manualIndepGroupsN2 + ", alternative=\"" + testHypothesisIndepGroups + "\", alpha = " + alpha + ")\n");
	}
	if(dataInput == 'manual' && groups == 'dep' && correlations == 'overlap') {
		echo("result <- cocor.dep.groups.overlap(r.jk=" + manualDepGroupsOverlapRJk + ", r.jh=" + manualDepGroupsOverlapRJh + ", r.kh=" + manualDepGroupsOverlapRKh + ", n=" + manualDepGroupsOverlapN + ", alternative=\"" + testHypothesisDepGroupsOverlap + "\", alpha = " + alpha + ")\n");
	}
	if(dataInput == 'manual' && groups == 'dep' && correlations == 'nonoverlap') {
		echo("result <- cocor.dep.groups.nonoverlap(r.jk=" + manualDepGroupsNonoverlapRJk + ", r.hm=" + manualDepGroupsNonoverlapRHm + ", r.jh=" + manualDepGroupsNonoverlapRJh + ", r.jm=" + manualDepGroupsNonoverlapRJm + ", r.kh=" + manualDepGroupsNonoverlapRKh + ", r.km=" + manualDepGroupsNonoverlapRKm + ", n=" + manualDepGroupsNonoverlapN + ", alternative=\"" + testHypothesisDepGroupsNonoverlap + "\", alpha=" + alpha + ")\n");
	}
	if(dataInput == 'variable' && groups == 'indep') {
		echo("result <- cocor.indep.groups(r1=" + variableIndepGroupsR1 + ", n1=" + variableIndepGroupsN1 + ", r2=" + variableIndepGroupsR2 + ", n2=" + variableIndepGroupsN2 + ", alternative=\"" + testHypothesisIndepGroups + "\", alpha = " + alpha + ")\n");
	}
	if(dataInput == 'variable' && groups == 'dep' && correlations == 'overlap') {
		echo("result <- cocor.dep.groups.overlap(r.jk=" + variableDepGroupsOverlapRJk + ", r.jh=" + variableDepGroupsOverlapRJh + ", r.kh=" + variableDepGroupsOverlapRKh + ", n=" + variableDepGroupsOverlapN + ", alternative=\"" + testHypothesisDepGroupsOverlap + "\", alpha=" + alpha + ")\n");
	}
	if(dataInput == 'variable' && groups == 'dep' && correlations == 'nonoverlap') {
		echo("result <- cocor.dep.groups.nonoverlap(r.jk=" + variableDepGroupsNonoverlapRJk + ", r.hm=" + variableDepGroupsNonoverlapRHm + ", r.jh=" + variableDepGroupsNonoverlapRJh + ", r.jm=" + variableDepGroupsNonoverlapRJm + ", r.kh=" + variableDepGroupsNonoverlapRKh + ", r.km=" + variableDepGroupsNonoverlapRKm + ", n=" + variableDepGroupsNonoverlapN + ", alternative=\"" + testHypothesisDepGroupsNonoverlap + "\", alpha=" + alpha + ")\n");
	}
}

function printout(){
	// printout the results
	echo("rk.header(\"Comparing correlations\")\n");

	echo("rk.print(result)\n");

}

