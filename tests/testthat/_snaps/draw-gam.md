# draw.gam issues message when all parametric terms are excluded

    Code
      plt <- draw(m_only_para, parametric = FALSE)
    Message
      i Unable to draw any of the model terms.

# draw.gam works for a parametric only model

    Code
      plt <- draw(m_only_para, parametric = TRUE, angle = 90, rug = FALSE, data = df_2_fac)

