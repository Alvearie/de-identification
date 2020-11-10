package com.ibm.whc.deid.providers.identifiers;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import com.ibm.whc.deid.shared.exception.DeidException;
import com.ibm.whc.deid.shared.pojo.config.DeidConfig;
import com.ibm.whc.deid.shared.util.ConfigUtil;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

public class IdentifierFactoryUtil {
	private static DeidConfig deidConfig = null;

	private static BuiltInIdentifierFactory identifierFactory = null;

	private static LogManager log = LogManager.getInstance();

	// get the static config object for the environment
	private static DeidConfig getDeidConfig() throws DeidException {
		if (deidConfig == null) {
			deidConfig = ConfigUtil.getDeidConfig();
		}
		return deidConfig;
	}

	public static BuiltInIdentifierFactory getIdentifierFactory() {

		if (identifierFactory == null) {
			try {
				DeidConfig deidConfig = getDeidConfig();

				@SuppressWarnings("unchecked")
				Constructor<? extends BuiltInIdentifierFactory> constructor = (Constructor<? extends BuiltInIdentifierFactory>) Class
						.forName(deidConfig.getIdentifierFactoryClass()).getConstructor();
				identifierFactory = constructor.newInstance();
			} catch (NoSuchMethodException | ClassNotFoundException | IllegalAccessException | InstantiationException
					| InvocationTargetException | DeidException e) {
				log.logError(LogCodes.WPH1013E, e);
				throw new RuntimeException(e.getMessage());
			}
		}

		return identifierFactory;
	}

}
