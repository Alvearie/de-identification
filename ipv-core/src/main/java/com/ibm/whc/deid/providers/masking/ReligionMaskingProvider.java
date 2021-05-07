/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.models.Religion;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.ReligionMaskingProviderConfig;
import com.ibm.whc.deid.util.ManagerFactory;
import com.ibm.whc.deid.util.ReligionManager;

/**
 * The religion privacy provider.
 */
public class ReligionMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = 4551913201853798655L;

  protected transient volatile ReligionManager religionResourceManager = null;

  public ReligionMaskingProvider(ReligionMaskingProviderConfig configuration, String tenantId,
      String localizationProperty) {
    super(tenantId, localizationProperty, configuration);
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    Religion religion = getReligionManager().getRandomValue();
    return religion == null ? null : religion.getName();
  }

  protected ReligionManager getReligionManager() {
    if (religionResourceManager == null) {
      religionResourceManager = (ReligionManager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.RELIGION, null, localizationProperty);
    }
    return religionResourceManager;
  }
}
