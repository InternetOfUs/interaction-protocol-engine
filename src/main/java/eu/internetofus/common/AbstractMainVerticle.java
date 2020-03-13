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

package eu.internetofus.common;

import eu.internetofus.common.api.AbstractAPIVerticle;
import eu.internetofus.common.persitences.AbstractPersistenceVerticle;
import eu.internetofus.common.services.AbstractServicesVerticle;
import io.vertx.core.AbstractVerticle;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Promise;

/**
 * The common verticle to start the component of the WeNet Module.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public abstract class AbstractMainVerticle extends AbstractVerticle {

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void start(Promise<Void> startPromise) throws Exception {

		final DeploymentOptions options = new DeploymentOptions(this.config()).setConfig(this.config());
		this.vertx.deployVerticle(this.getPersistenceVerticleClass(), options, deployPersistence -> {

			if (deployPersistence.failed()) {

				startPromise.fail(deployPersistence.cause());

			} else {

				this.vertx.deployVerticle(this.getServiceVerticleClass(), options, deployService -> {

					if (deployService.failed()) {

						startPromise.fail(deployService.cause());

					} else {

						this.vertx.deployVerticle(this.getAPIVerticleClass(), options, deployAPI -> {

							if (deployAPI.failed()) {

								startPromise.fail(deployAPI.cause());

							} else {

								startPromise.complete();
							}
						});

					}
				});
			}

		});

	}

	/**
	 * Return the vertice class to start the persistence repositories.
	 *
	 * @return the class to start the persistence repositories.
	 */
	protected abstract Class<? extends AbstractPersistenceVerticle> getPersistenceVerticleClass();

	/**
	 * Return the vertice class to start and bind the API web services.
	 *
	 * @return the class that bind the API.
	 */
	protected abstract Class<? extends AbstractAPIVerticle> getAPIVerticleClass();

	/**
	 * Return the vertice class to start the services.
	 *
	 * @return the class that start the services.
	 */
	protected abstract Class<? extends AbstractServicesVerticle> getServiceVerticleClass();

}
