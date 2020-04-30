/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine;

import eu.internetofus.common.AbstractMainVerticle;
import eu.internetofus.common.api.AbstractAPIVerticle;
import eu.internetofus.common.persitences.AbstractPersistenceVerticle;
import eu.internetofus.common.services.AbstractServicesVerticle;
import eu.internetofus.wenet_interaction_protocol_engine.api.APIVerticle;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.PersistenceVerticle;
import eu.internetofus.wenet_interaction_protocol_engine.services.ServicesVerticle;
import io.vertx.core.Promise;

/**
 * The Main verticle that deploy the necessary verticles for the WeNet
 * interactiomn protocol engine.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class MainVerticle extends AbstractMainVerticle {

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void start(Promise<Void> startPromise) throws Exception {

		EngineWorker.deploy(this.vertx, this.config(), deploy -> {

			if (deploy.failed()) {

				startPromise.fail(deploy.cause());

			} else {

				try {

					MainVerticle.super.start(startPromise);

				} catch (final Exception cause) {

					startPromise.fail(cause);
				}
			}

		});

	}

	/**
	 * {@inheritDoc}
	 *
	 * @see ServicesVerticle
	 */
	@Override
	protected Class<? extends AbstractServicesVerticle> getServiceVerticleClass() {

		return ServicesVerticle.class;

	}

	/**
	 * {@inheritDoc}
	 *
	 * @see PersistenceVerticle
	 */
	@Override
	protected Class<? extends AbstractPersistenceVerticle> getPersistenceVerticleClass() {

		return PersistenceVerticle.class;

	}

	/**
	 * {@inheritDoc}
	 *
	 * @see APIVerticle
	 */
	@Override
	protected Class<? extends AbstractAPIVerticle> getAPIVerticleClass() {

		return APIVerticle.class;

	}

}
