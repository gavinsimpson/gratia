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
      assemble(m_2_fac, parametric = TRUE, data = df_2_fac)
    Message
      -- Assembly of 6 plots --
      
      1. s(x0)
      2. s(x1)
      3. s(x2)
      4. fac
      5. fac:ff
      6. ff
      

# assemble.gam works for m_para_sm with angled labels

    Code
      assemble(m_para_sm, parametric = TRUE, data = df_2_fac)
    Message
      -- Assembly of 6 plots --
      
      1. s(x1)
      2. s(x2)
      3. fac
      4. fac:ff
      5. ff
      6. x0
      

