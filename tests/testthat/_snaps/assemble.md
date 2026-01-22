# assemble.gam works for m_1_smooth

    Code
      assemble(m_1_smooth)
    Message
      
      -- Assembly of 1 plots --
      
      1. s(x0)
      

# assemble.gam works for m_gam with angled labels

    Code
      assemble(m_gam)
    Message
      -- Assembly of 4 plots --
      
      1. s(x0)
      2. s(x1)
      3. s(x2)
      4. s(x3)
      

# assemble.gam works for m_2_fac with angled labels

    Code
      assemble(m_2_fac, parametric = TRUE, envir = teardown_env(), data = df_2_fac)
    Message
      i Interaction terms are not currently supported.
      
      -- Assembly of 5 plots --
      
      1. s(x0)
      2. s(x1)
      3. s(x2)
      4. fac
      5. ff
      

# assemble.gam works for m_para_sm with angled labels

    Code
      assemble(m_para_sm, parametric = TRUE, envir = teardown_env(), data = df_2_fac)
    Message
      i Interaction terms are not currently supported.
      
      -- Assembly of 5 plots --
      
      1. s(x1)
      2. s(x2)
      3. fac
      4. ff
      5. x0
      

