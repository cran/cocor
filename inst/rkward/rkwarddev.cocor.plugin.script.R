local({
  # set the output directory to overwrite the actual plugin
  output.dir <- "~/cocor/" # tempdir()
  overwrite <- TRUE

  require(rkwarddev)

  variable.selector <- rk.XML.varselector(id.name="vars")

  data.input <- rk.XML.radio("Data input",
    id.name="data_input",
    options=list(
      "Enter values manually"=c(val="manual"),
      "Use variables"=c(val="variable")
    )
  )

  groups <- rk.XML.radio("The two correlations are based on",
    id.name="groups",
    options=list(
      "two independent groups"=c(val="indep"),
      "two dependent groups (e.g., same group)"=c(val="dep")
    )
  )

  correlations <- rk.XML.radio("The two correlations are",
    id.name="correlations",
    options=list(
      "overlapping"=c(val="overlap"),
      "nonoverlapping"=c(val="nonoverlap")
    )
  )
  text.tailed.test <- "Do you want to conduct a one- or two-tailed test?"

  test.hypothesis.indep.groups <- rk.XML.radio(text.tailed.test,
    id.name="test_hypothesis_indep_groups",
    options=list(
      "Two-tailed: r1 is not equal to r2"=c(val="two.sided"),
      "One-tailed: r1 is greater than r2"=c(val="greater"),
      "One-tailed: r1 is less than r2 "=c(val="less")
    )
  )

  test.hypothesis.dep.groups.overlap <- rk.XML.radio(text.tailed.test,
    id.name="test_hypothesis_dep_groups_overlap",
    options=list(
      "Two-tailed: r.jk is not equal to r.jh"=c(val="two.sided"),
      "One-tailed: r.jk is greater than r.jh"=c(val="greater"),
      "One-tailed: r.jk is less than r.jh"=c(val="less")
    )
  )

  test.hypothesis.dep.groups.nonoverlap <- rk.XML.radio(text.tailed.test,
    id.name="test_hypothesis_dep_groups_nonoverlap",
    options=list(
      "Two-tailed: r.jk is not equal to r.hm"=c(val="two.sided"),
      "One-tailed: r.jk is greater than r.hm"=c(val="greater"),
      "One-tailed: r.jk is less than r.hm"=c(val="less")
    )
  )

  alpha <- rk.XML.spinbox("Alpha level", min=0, max=1, id.name="alpha", initial=.05)
  alpha.frame <- rk.XML.frame(alpha, label="Please choose an alpha level:")

  text.correlations <- "Please provide the correlations you want to compare:"
  text.related.correlation <- rk.XML.text("To assess the significance of the difference between the two dependent correlations, you need to provide the correlation between k and h:<br /><br />")
  text.related.correlations <- rk.XML.text("To assess the significance of the difference between the two dependent correlations, you need to provide additional related correlations:<br /><br />")
  text.group.size <- "Please indicate the size of your sample:"
  text.group.sizes <- "Please indicate the size of your samples:"

  ##manual input
  manual.indep.groups.r1 <- rk.XML.spinbox("Correlation r1", min=-1, max=1, id.name="manual_indep_groups_r1")
  manual.indep.groups.r2 <- rk.XML.spinbox("Correlation r2", min=-1, max=1, id.name="manual_indep_groups_r2")
  manual.indep.groups.n1 <- rk.XML.spinbox("Group size n1", min=3, real=FALSE, initial=30, id.name="manual_indep_groups_n1")
  manual.indep.groups.n2 <- rk.XML.spinbox("Group size n2", min=3, real=FALSE, initial=30, id.name="manual_indep_groups_n2")
  manual.indep.groups.cor.frame <- rk.XML.frame(manual.indep.groups.r1, manual.indep.groups.r2, label=text.correlations, id.name="manual_indep_groups_cor_frame")
  manual.indep.groups.group.size.frame <- rk.XML.frame(manual.indep.groups.n1, manual.indep.groups.n2, label=text.group.sizes, id.name="manual_indep_groups_group_size_frame")
  manual.indep.groups.frame <- rk.XML.frame(manual.indep.groups.cor.frame, manual.indep.groups.group.size.frame, label="", id.name="manual_indep_groups_frame")

  manual.dep.groups.overlap.r.jk <- rk.XML.spinbox("Correlation j ~ k", min=-1, max=1, id.name="manual_dep_groups_overlap_r_jk")
  manual.dep.groups.overlap.r.jh <- rk.XML.spinbox("Correlation j ~ h", min=-1, max=1, id.name="manual_dep_groups_overlap_r_jh")
  manual.dep.groups.overlap.r.kh <- rk.XML.spinbox("Correlation k ~ h", min=-1, max=1, id.name="manual_dep_groups_overlap_r_kh")
  manual.dep.groups.overlap.n <- rk.XML.spinbox("Group size n", min=3, real=FALSE, initial=30, id.name="manual_dep_groups_overlap_n")
  manual.dep.groups.overlap.cor.frame <- rk.XML.frame(manual.dep.groups.overlap.r.jk, manual.dep.groups.overlap.r.jh, label=text.correlations, id.name="manual_dep_groups_overlap_cor_frame")
  manual.dep.groups.overlap.related.cor.frame <- rk.XML.frame(text.related.correlation, manual.dep.groups.overlap.r.kh, id.name="manual_dep_groups_overlap_related_cor_frame")
  manual.dep.groups.overlap.group.size.frame <- rk.XML.frame(manual.dep.groups.overlap.n, label=text.group.size, id.name="manual_dep_groups_overlap_group_size_frame")
  manual.dep.groups.overlap.frame <- rk.XML.frame(manual.dep.groups.overlap.cor.frame, manual.dep.groups.overlap.related.cor.frame, manual.dep.groups.overlap.group.size.frame, label="", id.name="manual_dep_groups_overlap_frame")

  manual.dep.groups.nonoverlap.r.jk <- rk.XML.spinbox("Correlation j ~ k", min=-1, max=1, id.name="manual_dep_groups_nonoverlap_r_jk")
  manual.dep.groups.nonoverlap.r.hm <- rk.XML.spinbox("Correlation h ~ m", min=-1, max=1, id.name="manual_dep_groups_nonoverlap_r_hm")
  manual.dep.groups.nonoverlap.r.jh <- rk.XML.spinbox("Correlation j ~ h", min=-1, max=1, id.name="manual_dep_groups_nonoverlap_r_jh")
  manual.dep.groups.nonoverlap.r.jm <- rk.XML.spinbox("Correlation j ~ m", min=-1, max=1, id.name="manual_dep_groups_nonoverlap_r_jm")
  manual.dep.groups.nonoverlap.r.kh <- rk.XML.spinbox("Correlation k ~ h", min=-1, max=1, id.name="manual_dep_groups_nonoverlap_r_kh")
  manual.dep.groups.nonoverlap.r.km <- rk.XML.spinbox("Correlation k ~ m", min=-1, max=1, id.name="manual_dep_groups_nonoverlap_r_km")
  manual.dep.groups.nonoverlap.n <- rk.XML.spinbox("Group size n", min=3, real=FALSE, initial=30, id.name="manual_dep_groups_nonoverlap_n")
  manual.dep.groups.nonoverlap.cor.frame <- rk.XML.frame(manual.dep.groups.nonoverlap.r.jk, manual.dep.groups.nonoverlap.r.hm, label=text.correlations, id.name="manual_dep_groups_nonoverlap_cor_frame")
  manual.dep.groups.nonoverlap.related.cor.frame <- rk.XML.frame(text.related.correlations, manual.dep.groups.nonoverlap.r.jh, manual.dep.groups.nonoverlap.r.jm, manual.dep.groups.nonoverlap.r.kh, manual.dep.groups.nonoverlap.r.km, id.name="manual_dep_groups_nonoverlap_related_cor_frame")
  manual.dep.groups.nonoverlap.group.size.frame <- rk.XML.frame(manual.dep.groups.nonoverlap.n, label=text.group.size, id.name="manual_dep_groups_nonoverlap_group_size_frame")
  manual.dep.groups.nonoverlap.frame <- rk.XML.frame(manual.dep.groups.nonoverlap.cor.frame, manual.dep.groups.nonoverlap.related.cor.frame, manual.dep.groups.nonoverlap.group.size.frame, id.name="manual_dep_groups_nonoverlap_frame")

  ##variable input
  variable.indep.groups.r1 <- rk.XML.varslot("Correlation r1", source=variable.selector, types="number", id.name="variable_indep_groups_r1")
  variable.indep.groups.r2 <- rk.XML.varslot("Correlation r2", source=variable.selector, types="number", id.name="variable_indep_groups_r2")
  variable.indep.groups.n1 <- rk.XML.varslot("Group size n1", source=variable.selector, types="number", id.name="variable_indep_groups_n1")
  variable.indep.groups.n2 <- rk.XML.varslot("Group size n2", source=variable.selector, types="number", id.name="variable_indep_groups_n2")
  variable.indep.groups.cor.frame <- rk.XML.frame(variable.indep.groups.r1, variable.indep.groups.r2, label=text.correlations, id.name="variable_indep_groups_cor_frame")
  variable.indep.groups.group.size.frame <- rk.XML.frame(variable.indep.groups.n1, variable.indep.groups.n2, label=text.group.sizes, id.name="variable_indep_groups_group_size_frame")
  variable.indep.groups.frame <- rk.XML.frame(variable.indep.groups.cor.frame, variable.indep.groups.group.size.frame, label="", id.name="variable_indep_groups_frame")

  variable.dep.groups.overlap.r.jk <- rk.XML.varslot("Correlation j ~ k", source=variable.selector, types="number", id.name="variable_dep_groups_overlap_r_jk")
  variable.dep.groups.overlap.r.jh <- rk.XML.varslot("Correlation j ~ h", source=variable.selector, types="number", id.name="variable_dep_groups_overlap_r_jh")
  variable.dep.groups.overlap.r.kh <- rk.XML.varslot("Correlation k ~ h", source=variable.selector, types="number", id.name="variable_dep_groups_overlap_r_kh")
  variable.dep.groups.overlap.n <- rk.XML.varslot("Group size n", source=variable.selector, types="number", id.name="variable_dep_groups_overlap_n")
  variable.dep.groups.overlap.cor.frame <- rk.XML.frame(variable.dep.groups.overlap.r.jk, variable.dep.groups.overlap.r.jh, label=text.correlations, id.name="variable_dep_groups_overlap_cor_frame")
  variable.dep.groups.overlap.related.cor.frame <- rk.XML.frame(text.related.correlation, variable.dep.groups.overlap.r.kh, id.name="variable_dep_groups_overlap_related_cor_frame")
  variable.dep.groups.overlap.group.size.frame <- rk.XML.frame(variable.dep.groups.overlap.n, label=text.group.size, id.name="variable_dep_groups_overlap_group_size_frame")
  variable.dep.groups.overlap.frame <- rk.XML.frame(variable.dep.groups.overlap.cor.frame, variable.dep.groups.overlap.related.cor.frame, variable.dep.groups.overlap.group.size.frame, label="", id.name="variable_dep_groups_overlap_frame")

  variable.dep.groups.nonoverlap.r.jk <- rk.XML.varslot("Correlation j ~ k", source=variable.selector, types="number", id.name="variable_dep_groups_nonoverlap_r_jk")
  variable.dep.groups.nonoverlap.r.hm <- rk.XML.varslot("Correlation h ~ m", source=variable.selector, types="number", id.name="variable_dep_groups_nonoverlap_r_hm")
  variable.dep.groups.nonoverlap.r.jh <- rk.XML.varslot("Correlation j ~ h", source=variable.selector, types="number", id.name="variable_dep_groups_nonoverlap_r_jh")
  variable.dep.groups.nonoverlap.r.jm <- rk.XML.varslot("Correlation j ~ m", source=variable.selector, types="number", id.name="variable_dep_groups_nonoverlap_r_jm")
  variable.dep.groups.nonoverlap.r.kh <- rk.XML.varslot("Correlation k ~ h", source=variable.selector, types="number", id.name="variable_dep_groups_nonoverlap_r_kh")
  variable.dep.groups.nonoverlap.r.km <- rk.XML.varslot("Correlation k ~ m", source=variable.selector, types="number", id.name="variable_dep_groups_nonoverlap_r_km")
  variable.dep.groups.nonoverlap.n <- rk.XML.varslot("Group size n", source=variable.selector, types="number", id.name="variable_dep_groups_nonoverlap_n")
  variable.dep.groups.nonoverlap.cor.frame <- rk.XML.frame(variable.dep.groups.nonoverlap.r.jk, variable.dep.groups.nonoverlap.r.hm, label=text.correlations, id.name="variable_dep_groups_nonoverlap_cor_frame")
  variable.dep.groups.nonoverlap.related.cor.frame <- rk.XML.frame(text.related.correlations, variable.dep.groups.nonoverlap.r.jh, variable.dep.groups.nonoverlap.r.jm, variable.dep.groups.nonoverlap.r.kh, variable.dep.groups.nonoverlap.r.km, id.name="variable_dep_groups_nonoverlap_related_cor_frame")
  variable.dep.groups.nonoverlap.group.size.frame <- rk.XML.frame(variable.dep.groups.nonoverlap.n, label=text.group.size, id.name="variable_dep_groups_nonoverlap_group_size_frame")
  variable.dep.groups.nonoverlap.frame <- rk.XML.frame(variable.dep.groups.nonoverlap.cor.frame, variable.dep.groups.nonoverlap.related.cor.frame, variable.dep.groups.nonoverlap.group.size.frame, label="", id.name="variable_dep_groups_nonoverlap_frame")

  wizard.correlations.page <- rk.XML.page(
    id.name="wizard_correlations_page",
    rk.XML.text("Are the two correlations overlapping, i.e., do they have one variable in common?"),
    correlations,
    rk.XML.frame(label="Examples",
      rk.XML.text("Overlapping:<br />Correlation 1: age ~ intelligence<br />Correlation 2: age ~ shoe size<br /><br />These are overlapping correlations because the same variable 'age' is part of both correlations.<br /><br />Nonoverlapping:<br />Correlation 1: age ~ intelligence<br />Correlation 2: body mass index ~ shoe size<br /><br />These are nonoverlapping correlations because no variable is part of both correlations.<br /><br /><br />")
    ),
    rk.XML.stretch()
  )

  wizard <- rk.XML.wizard(
    label="Comparing correlations",
    rk.XML.page(
      rk.XML.text("Are the two correlations based on two independent or on two dependent groups? (If the data were taken from measurements of the same individuals, the groups are dependent.)<br />"),
      groups,
      rk.XML.stretch()
    ),
    wizard.correlations.page,
    rk.XML.page(
      rk.XML.text("Do you want to type the data in manually or are the data available in single variables?"),
      data.input,
      rk.XML.stretch()
    ),
    rk.XML.page(
      rk.XML.row(
        rk.XML.col(variable.selector),
        rk.XML.col(
          manual.indep.groups.frame, manual.dep.groups.overlap.frame, manual.dep.groups.nonoverlap.frame,
          variable.indep.groups.frame, variable.dep.groups.overlap.frame, variable.dep.groups.nonoverlap.frame,
          rk.XML.stretch()
        )
      )
    ),
    rk.XML.page(
      test.hypothesis.indep.groups,
      test.hypothesis.dep.groups.overlap,
      test.hypothesis.dep.groups.nonoverlap,
      alpha.frame,
      rk.XML.stretch()
    )
  )

  ##logic
  #convert single
  manual.input.convert <- rk.XML.convert(sources=list(string=data.input), mode=c(equals="manual"), id.name="manual_input_convert")
  variable.input.convert <- rk.XML.convert(sources=list(string=data.input), mode=c(equals="variable"), id.name="variable_input_convert")

  indep.groups.convert <- rk.XML.convert(sources=list(string=groups), mode=c(equals="indep"), id.name="indep_groups_convert")
  dep.groups.convert <- rk.XML.convert(sources=list(string=groups), mode=c(equals="dep"), id.name="dep_groups_convert")

  overlap.correlations.convert <- rk.XML.convert(sources=list(string=correlations), mode=c(equals="overlap"), id.name="overlap_correlations_convert")
  nonoverlap.correlations.convert <- rk.XML.convert(sources=list(string=correlations), mode=c(equals="nonoverlap"), id.name="nonoverlap_correlations_convert")

  #convert multiple
  dep.groups.and.nonoverlap.correlations.convert <- rk.XML.convert(sources=list(dep.groups.convert, nonoverlap.correlations.convert), mode=c(and=""), id.name="dep_groups_and_nonoverlap_correlations_convert")
  dep.groups.and.overlap.correlations.convert <- rk.XML.convert(sources=list(dep.groups.convert, overlap.correlations.convert), mode=c(and=""), id.name="dep_groups_and_overlap_correlations_convert")

  manual.and.indep.groups.convert <- rk.XML.convert(sources=list(manual.input.convert, indep.groups.convert), mode=c(and=""), id.name="manual_and_indep_groups_convert")
  manual.and.dep.groups.and.nonoverlap.correlations.convert <- rk.XML.convert(sources=list(manual.input.convert, dep.groups.convert, nonoverlap.correlations.convert), mode=c(and=""), id.name="manual_and_dep_groups_and_nonoverlap_correlations_convert")
  manual.and.dep.groups.and.overlap.correlations.convert <- rk.XML.convert(sources=list(manual.input.convert, dep.groups.convert, overlap.correlations.convert), mode=c(and=""), id.name="manual_and_dep_groups_and_overlap_correlations_convert")

  variable.and.indep.groups.convert <- rk.XML.convert(sources=list(variable.input.convert, indep.groups.convert), mode=c(and=""), id.name="variable_and_indep_groups_convert")
  variable.and.dep.groups.and.nonoverlap.correlations.convert <- rk.XML.convert(sources=list(variable.input.convert, dep.groups.convert, nonoverlap.correlations.convert), mode=c(and=""), id.name="variable_and_dep_groups_and_nonoverlap_correlations_convert")
  variable.and.dep.groups.and.overlap.correlations.convert <- rk.XML.convert(sources=list(variable.input.convert, dep.groups.convert, overlap.correlations.convert), mode=c(and=""), id.name="variable_and_dep_groups_and_overlap_correlations_convert")

  #connect
  manual.indep.groups.frame.visible.connect <- rk.XML.connect(governor=manual.and.indep.groups.convert, client=manual.indep.groups.frame, set="visible")
  manual.dep.groups.nonoverlap.frame.visible.connect <- rk.XML.connect(governor=manual.and.dep.groups.and.nonoverlap.correlations.convert, client=manual.dep.groups.nonoverlap.frame, set="visible")
  manual.dep.groups.overlap.frame.visible.connect <- rk.XML.connect(governor=manual.and.dep.groups.and.overlap.correlations.convert, client=manual.dep.groups.overlap.frame, set="visible")

  variable.indep.groups.frame.visible.connect <- rk.XML.connect(governor=variable.and.indep.groups.convert, client=variable.indep.groups.frame, set="visible")
  variable.dep.groups.nonoverlap.frame.visible.connect <- rk.XML.connect(governor=variable.and.dep.groups.and.nonoverlap.correlations.convert, client=variable.dep.groups.nonoverlap.frame, set="visible")
  variable.dep.groups.overlap.frame.visible.connect <- rk.XML.connect(governor=variable.and.dep.groups.and.overlap.correlations.convert, client=variable.dep.groups.overlap.frame, set="visible")

  test.hypothesis.indep.groups.connect <- rk.XML.connect(governor=indep.groups.convert, client=test.hypothesis.indep.groups, set="visible")
  test.hypothesis.dep.groups.overlap.connect <- rk.XML.connect(governor=dep.groups.and.overlap.correlations.convert, client=test.hypothesis.dep.groups.overlap, set="visible")
  test.hypothesis.dep.groups.nonoverlap.connect <- rk.XML.connect(governor=dep.groups.and.nonoverlap.correlations.convert, client=test.hypothesis.dep.groups.nonoverlap, set="visible")

  variable.selector.connect <- rk.XML.connect(governor=variable.input.convert, client=variable.selector, set="visible")

  #wizard
  wizard.correlations.connect <- rk.XML.connect(governor=dep.groups.convert, client=wizard.correlations.page, set="visible")

  #require
  variable.indep.groups.r1.required.connect <- rk.XML.connect(governor=variable.and.indep.groups.convert, client=variable.indep.groups.r1, set="required")
  variable.indep.groups.n1.required.connect <- rk.XML.connect(governor=variable.and.indep.groups.convert, client=variable.indep.groups.n1, set="required")
  variable.indep.groups.r2.required.connect <- rk.XML.connect(governor=variable.and.indep.groups.convert, client=variable.indep.groups.r2, set="required")
  variable.indep.groups.n2.required.connect <- rk.XML.connect(governor=variable.and.indep.groups.convert, client=variable.indep.groups.n2, set="required")

  variable.dep.groups.overlap.r.jk.required.connect <- rk.XML.connect(governor=variable.and.dep.groups.and.overlap.correlations.convert, client=variable.dep.groups.overlap.r.jk, set="required")
  variable.dep.groups.overlap.r.jh.required.connect <- rk.XML.connect(governor=variable.and.dep.groups.and.overlap.correlations.convert, client=variable.dep.groups.overlap.r.jh, set="required")
  variable.dep.groups.overlap.r.kh.required.connect <- rk.XML.connect(governor=variable.and.dep.groups.and.overlap.correlations.convert, client=variable.dep.groups.overlap.r.kh, set="required")
  variable.dep.groups.overlap.n.required.connect <- rk.XML.connect(governor=variable.and.dep.groups.and.overlap.correlations.convert, client=variable.dep.groups.overlap.n, set="required")

  variable.dep.groups.nonoverlap.r.jk.required.connect <- rk.XML.connect(governor=variable.and.dep.groups.and.nonoverlap.correlations.convert, client=variable.dep.groups.nonoverlap.r.jk, set="required")
  variable.dep.groups.nonoverlap.r.hm.required.connect <- rk.XML.connect(governor=variable.and.dep.groups.and.nonoverlap.correlations.convert, client=variable.dep.groups.nonoverlap.r.hm, set="required")
  variable.dep.groups.nonoverlap.r.jh.required.connect <- rk.XML.connect(governor=variable.and.dep.groups.and.nonoverlap.correlations.convert, client=variable.dep.groups.nonoverlap.r.jh, set="required")
  variable.dep.groups.nonoverlap.r.jm.required.connect <- rk.XML.connect(governor=variable.and.dep.groups.and.nonoverlap.correlations.convert, client=variable.dep.groups.nonoverlap.r.jm, set="required")
  variable.dep.groups.nonoverlap.r.kh.required.connect <- rk.XML.connect(governor=variable.and.dep.groups.and.nonoverlap.correlations.convert, client=variable.dep.groups.nonoverlap.r.kh, set="required")
  variable.dep.groups.nonoverlap.r.km.required.connect <- rk.XML.connect(governor=variable.and.dep.groups.and.nonoverlap.correlations.convert, client=variable.dep.groups.nonoverlap.r.km, set="required")
  variable.dep.groups.nonoverlap.n.required.connect <- rk.XML.connect(governor=variable.and.dep.groups.and.nonoverlap.correlations.convert, client=variable.dep.groups.nonoverlap.n, set="required")

  logic <- rk.XML.logic(
    #convert single
    manual.input.convert,
    variable.input.convert,

    indep.groups.convert,
    dep.groups.convert,

    nonoverlap.correlations.convert,
    overlap.correlations.convert,

    #convert multiple
    dep.groups.and.nonoverlap.correlations.convert,
    dep.groups.and.overlap.correlations.convert,

    manual.and.indep.groups.convert,
    manual.and.dep.groups.and.nonoverlap.correlations.convert,
    manual.and.dep.groups.and.overlap.correlations.convert,

    variable.and.indep.groups.convert,
    variable.and.dep.groups.and.nonoverlap.correlations.convert,
    variable.and.dep.groups.and.overlap.correlations.convert,

    #connect
    manual.indep.groups.frame.visible.connect,
    manual.dep.groups.nonoverlap.frame.visible.connect,
    manual.dep.groups.overlap.frame.visible.connect,

    variable.indep.groups.frame.visible.connect,
    variable.dep.groups.nonoverlap.frame.visible.connect,
    variable.dep.groups.overlap.frame.visible.connect,

    variable.selector.connect,

    test.hypothesis.indep.groups.connect,
    test.hypothesis.dep.groups.overlap.connect,
    test.hypothesis.dep.groups.nonoverlap.connect,

    #wizard
    wizard.correlations.connect,

    #require
    variable.indep.groups.r1.required.connect,
    variable.indep.groups.n1.required.connect,
    variable.indep.groups.r2.required.connect,
    variable.indep.groups.n2.required.connect,

    variable.dep.groups.overlap.r.jk.required.connect,
    variable.dep.groups.overlap.r.jh.required.connect,
    variable.dep.groups.overlap.r.kh.required.connect,
    variable.dep.groups.overlap.n.required.connect,

    variable.dep.groups.nonoverlap.r.jk.required.connect,
    variable.dep.groups.nonoverlap.r.hm.required.connect,
    variable.dep.groups.nonoverlap.r.jh.required.connect,
    variable.dep.groups.nonoverlap.r.jm.required.connect,
    variable.dep.groups.nonoverlap.r.kh.required.connect,
    variable.dep.groups.nonoverlap.r.km.required.connect,
    variable.dep.groups.nonoverlap.n.required.connect
  )

  JS.calc <- rk.paste.JS(
    ##manual input
    ite(id(data.input, " == 'manual' && ", groups, " == 'indep'"), echo("result <- cocor.indep.groups(r1=", manual.indep.groups.r1, ", n1=", manual.indep.groups.n1, ", r2=", manual.indep.groups.r2, ", n2=", manual.indep.groups.n2, ", alternative=\"", test.hypothesis.indep.groups, "\", alpha = ",  alpha, ")\n")),

    ite(id(data.input, " == 'manual' && ", groups, " == 'dep' && ", correlations, " == 'overlap'"), echo("result <- cocor.dep.groups.overlap(r.jk=", manual.dep.groups.overlap.r.jk, ", r.jh=", manual.dep.groups.overlap.r.jh, ", r.kh=", manual.dep.groups.overlap.r.kh, ", n=", manual.dep.groups.overlap.n, ", alternative=\"", test.hypothesis.dep.groups.overlap, "\", alpha = ",  alpha, ")\n")),

    ite(id(data.input, " == 'manual' && ", groups, " == 'dep' && ", correlations, " == 'nonoverlap'"), echo("result <- cocor.dep.groups.nonoverlap(r.jk=", manual.dep.groups.nonoverlap.r.jk, ", r.hm=", manual.dep.groups.nonoverlap.r.hm, ", r.jh=", manual.dep.groups.nonoverlap.r.jh, ", r.jm=", manual.dep.groups.nonoverlap.r.jm, ", r.kh=", manual.dep.groups.nonoverlap.r.kh, ", r.km=", manual.dep.groups.nonoverlap.r.km,", n=", manual.dep.groups.nonoverlap.n, ", alternative=\"", test.hypothesis.dep.groups.nonoverlap, "\", alpha=",  alpha, ")\n")),

    ##variable input
    ite(id(data.input, " == 'variable' && ", groups, " == 'indep'"), echo("result <- cocor.indep.groups(r1=", variable.indep.groups.r1, ", n1=", variable.indep.groups.n1, ", r2=", variable.indep.groups.r2, ", n2=", variable.indep.groups.n2, ", alternative=\"", test.hypothesis.indep.groups, "\", alpha = ",  alpha, ")\n")),

    ite(id(data.input, " == 'variable' && ", groups, " == 'dep' && ", correlations, " == 'overlap'"), echo("result <- cocor.dep.groups.overlap(r.jk=", variable.dep.groups.overlap.r.jk, ", r.jh=", variable.dep.groups.overlap.r.jh, ", r.kh=", variable.dep.groups.overlap.r.kh, ", n=", variable.dep.groups.overlap.n, ", alternative=\"", test.hypothesis.dep.groups.overlap, "\", alpha=",  alpha, ")\n")),

    ite(id(data.input, " == 'variable' && ", groups, " == 'dep' && ", correlations, " == 'nonoverlap'"), echo("result <- cocor.dep.groups.nonoverlap(r.jk=", variable.dep.groups.nonoverlap.r.jk, ", r.hm=", variable.dep.groups.nonoverlap.r.hm, ", r.jh=", variable.dep.groups.nonoverlap.r.jh, ", r.jm=", variable.dep.groups.nonoverlap.r.jm, ", r.kh=", variable.dep.groups.nonoverlap.r.kh, ", r.km=", variable.dep.groups.nonoverlap.r.km,", n=", variable.dep.groups.nonoverlap.n, ", alternative=\"", test.hypothesis.dep.groups.nonoverlap, "\", alpha=",  alpha, ")\n")),
    level=2
  )

  JS.print <- rk.paste.JS(echo("rk.print(result)\n"), level=2)

  rkh <- list(
    summary=rk.rkh.summary(text="Compare two correlation coefficients based on either independent or dependent groups."),
    settings=rk.rkh.settings(
      rk.rkh.setting(groups, "Indicate if the correlations are based on independent or dependent groups."),
      rk.rkh.setting(correlations, "State whether or not the correlations are overlapping and have one variable in common."),
      rk.rkh.setting(test.hypothesis.indep.groups, "Select a if the test should be one- or two-sided."),
      rk.rkh.setting(data.input, "Choose whether you want to type in the values manually or retrieve them from existing variables."),
      rk.rkh.setting(manual.indep.groups.frame, "Type in the required values or retrieve them from existing variables.")
    )
  )

  plugin.dir <<- rk.plugin.skeleton(
    "cocor",
    path=output.dir,
    provides=c("logic", "wizard"),
    xml=list(logic=logic, wizard=wizard),
    rkh=rkh,
    js=list(
      require="cocor",
      results.header="\"Comparing correlations\"",
      calculate=JS.calc,
      printout=JS.print
    ),
    pluginmap=list(name="Comparing correlations", hierarchy=list("analysis", "correlation")),
    load=TRUE,
#     edit=TRUE,
#     show=TRUE,
    overwrite=overwrite
  )
})
