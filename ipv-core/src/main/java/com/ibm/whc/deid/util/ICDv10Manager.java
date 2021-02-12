/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import com.ibm.whc.deid.models.ICD;
import com.ibm.whc.deid.models.ICDFormat;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.localization.Resources;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

/** The type Ic dv 10 manager. */
public class ICDv10Manager implements Manager, Serializable {
  /** */
  private static final long serialVersionUID = -7207643604541499595L;

  protected static final Collection<ResourceEntry> resourceICDList =
      LocalizationManager.getInstance().getResources(Resource.ICDV10);

  protected final MapWithRandomPick<String, ICD> icdByCodeMap;
  protected final MapWithRandomPick<String, ICD> icdByNameMap;

  protected int resourceInDbCount = 0;

  private static LogManager logger = LogManager.getInstance();
  protected transient final Resources resourceType = Resource.ICDV10;

  protected final String tenantId;

  /**
   * Instantiates a new Ic dv 10 manager.
   *
   * @param tenantId
   */
  public ICDv10Manager(String tenantId) {
    this.tenantId = tenantId;
    this.icdByCodeMap = new MapWithRandomPick<>(new HashMap<String, ICD>());
    this.icdByNameMap = new MapWithRandomPick<>(new HashMap<String, ICD>());

    readResources(resourceType, tenantId);

    this.icdByCodeMap.setKeyList();
    this.icdByNameMap.setKeyList();
  }

  protected void readResources(Resources resourceType, String tenantId) {
    readICDList(resourceICDList);
  }

  protected Map<? extends String, ? extends String> readICDList(Collection<ResourceEntry> entries) {
    Map<String, String> names = new HashMap<>();

    for (ResourceEntry entry : entries) {
      InputStream inputStream = entry.createStream();
      try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
        for (CSVRecord line : reader) {
          String code = line.get(0);
          String fullName = line.get(1);
          String categoryCode = line.get(2);
          String categoryName = line.get(3);
          String chapterCode = line.get(4);
          String chapterName = line.get(5);

          ICD ICDv10CodeRepr = new ICD(code, fullName, fullName, chapterCode, chapterName,
              categoryCode, categoryName, ICDFormat.CODE);
          ICD ICDv10NameRepr = new ICD(code, fullName, fullName, chapterCode, chapterName,
              categoryCode, categoryName, ICDFormat.NAME);

          this.icdByCodeMap.getMap().put(code, ICDv10CodeRepr);
          this.icdByNameMap.getMap().put(fullName.toUpperCase(), ICDv10NameRepr);
        }
        inputStream.close();
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }

    return names;
  }

  @Override
  public String getRandomKey() {
    return this.icdByCodeMap.getRandomKey();
  }

  /**
   * Lookup icd icd.
   *
   * @param codeOrName the code or name
   * @return the icd
   */
  public ICD lookupICD(String codeOrName) {
    String key = codeOrName.toUpperCase();
    ICD icd;

    if ((icd = this.icdByCodeMap.getMap().get(key)) != null) {
      return icd;
    } else if ((icd = this.icdByNameMap.getMap().get(key)) != null) {
      return icd;
    }

    return null;
  }

  @Override
  public boolean isValidKey(String codeOrName) {
    return lookupICD(codeOrName) != null;
  }

}
