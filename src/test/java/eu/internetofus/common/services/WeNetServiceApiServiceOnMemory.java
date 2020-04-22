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

package eu.internetofus.common.services;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import eu.internetofus.common.api.models.Model;
import eu.internetofus.common.api.models.wenet.App;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.serviceproxy.ServiceBinder;

/**
 * Implementation of the {@link WeNetServiceApiService} that can be used for
 * unit testing.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class WeNetServiceApiServiceOnMemory implements WeNetServiceApiService {

	/**
	 * Register this service.
	 *
	 * @param vertx that contains the event bus to use.
	 */
	public static void register(Vertx vertx) {

		new ServiceBinder(vertx).setAddress(WeNetServiceApiService.ADDRESS).register(WeNetServiceApiService.class,
				new WeNetServiceApiServiceOnMemory());

	}

	/**
	 * The apps that has been stored on the service.
	 */
	private final Map<String, JsonObject> apps;

	/**
	 * Create the service.
	 */
	public WeNetServiceApiServiceOnMemory() {

		this.apps = new HashMap<>();

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public synchronized void createApp(JsonObject app, Handler<AsyncResult<JsonObject>> createHandler) {

		final App model = Model.fromJsonObject(app, App.class);
		if (model == null) {
			// bad app
			createHandler.handle(Future.failedFuture("Bad app to store"));

		} else {

			String id = app.getString("appId");
			if (id == null) {

				id = UUID.randomUUID().toString();
				app.put("appId", id);
			}

			if (this.apps.containsKey(id)) {

				createHandler.handle(Future.failedFuture("App already registered"));

			} else {

				this.apps.put(id, app);
				createHandler.handle(Future.succeededFuture(app));
			}
		}

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public synchronized void retrieveApp(String id, Handler<AsyncResult<JsonObject>> retrieveHandler) {

		final JsonObject app = this.apps.get(id);
		if (app == null) {

			retrieveHandler.handle(Future.failedFuture("No Application associated to the ID"));

		} else {

			retrieveHandler.handle(Future.succeededFuture(app));

		}

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public synchronized void deleteApp(String id, Handler<AsyncResult<Void>> deleteHandler) {

		final JsonObject app = this.apps.remove(id);
		if (app == null) {

			deleteHandler.handle(Future.failedFuture("No Application associated to the ID"));

		} else {

			deleteHandler.handle(Future.succeededFuture());

		}

	}

}
