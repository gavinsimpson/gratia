# draw.gam issues message for parametric only model

    Code
      plt <- draw(m_only_para)
    Message
      i Unable to draw any of the model terms.

# draw.gam works for a parametric only model

    Code
      plt <- draw(m_only_para, parametric = TRUE, angle = 90, rug = FALSE, data = df_2_fac,
        envir = teardown_env())
    Message
      i Interaction terms are not currently supported.

