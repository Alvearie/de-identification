/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.VINMaskingProviderConfig;
import com.ibm.whc.deid.util.ManagerFactory;
import com.ibm.whc.deid.util.RandomGenerators;
import com.ibm.whc.deid.util.VINManager;

public class VINMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = -6153333908702391325L;

  protected static final char[] allowedCharacters =
      "ABCDEFGHJKLMNPRSTUVWXYZ0123456789".toCharArray();

  protected final boolean preserveWMI;
  protected final boolean preserveVDS;
  protected final int unspecifiedValueHandling;
  protected final String unspecifiedValueReturnMessage;

  protected transient volatile VINManager vinResourceManager = null;

  public VINMaskingProvider(VINMaskingProviderConfig configuration, String tenantId,
      String localizationProperty) {
    super(tenantId, localizationProperty);
    this.preserveWMI = configuration.isWmiPreserve();
    this.preserveVDS = configuration.isVdsPreserve();
    this.unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();
  }

  private String randomVIN(VINManager vinManager) {
    String wmi = vinManager.getRandomWMI();
    String vds = RandomGenerators.randomUIDGeneratorWithInclusions(6, allowedCharacters);
    String vis = RandomGenerators.randomUIDGeneratorWithInclusions(8, allowedCharacters);
    return String.format("%s%s%s", wmi, vds, vis);
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    VINManager vinManager = getVINManager();

    if (!vinManager.isValidKey(identifier)) {
      debugFaultyInput("vinIdentifier");
      if (unspecifiedValueHandling == 2) {
        return this.randomVIN(vinManager);
      } else if (unspecifiedValueHandling == 3) {
        return unspecifiedValueReturnMessage;
      } else {
        return null;
      }
    }

    String wmi = this.preserveWMI ? identifier.substring(0, 3) : vinManager.getRandomWMI();
    String vds = this.preserveVDS ? identifier.substring(3, 9)
        : RandomGenerators.randomUIDGeneratorWithInclusions(6, allowedCharacters);
    String vis = RandomGenerators.randomUIDGeneratorWithInclusions(8, allowedCharacters);

    return wmi + vds + vis;
  }

  protected VINManager getVINManager() {
    if (vinResourceManager == null) {
      vinResourceManager = (VINManager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.WORLD_MANUFACTURERS_IDENTIFIER, null, localizationProperty);
    }
    return vinResourceManager;
  }
}
