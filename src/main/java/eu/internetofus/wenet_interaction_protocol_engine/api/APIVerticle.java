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
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
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

package eu.internetofus.wenet_interaction_protocol_engine.api;

import eu.internetofus.common.components.interaction_protocol_engine.WeNetInteractionProtocolEngine;
import eu.internetofus.common.components.interaction_protocol_engine.WeNetInteractionProtocolEngineClient;
import eu.internetofus.common.components.profile_manager.WeNetProfileManager;
import eu.internetofus.common.vertx.AbstractAPIVerticle;
import eu.internetofus.wenet_interaction_protocol_engine.api.help.Help;
import eu.internetofus.wenet_interaction_protocol_engine.api.help.HelpResource;
import eu.internetofus.wenet_interaction_protocol_engine.api.incentives.Incentives;
import eu.internetofus.wenet_interaction_protocol_engine.api.incentives.IncentivesResource;
import eu.internetofus.wenet_interaction_protocol_engine.api.messages.Messages;
import eu.internetofus.wenet_interaction_protocol_engine.api.messages.MessagesResource;
import eu.internetofus.wenet_interaction_protocol_engine.api.norms.Norms;
import eu.internetofus.wenet_interaction_protocol_engine.api.norms.NormsResource;
import eu.internetofus.wenet_interaction_protocol_engine.api.tasks.Tasks;
import eu.internetofus.wenet_interaction_protocol_engine.api.tasks.TasksResource;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.WebClient;
import io.vertx.ext.web.openapi.RouterBuilder;
import io.vertx.serviceproxy.ServiceBinder;

/**
 * The verticle that provide the manage the WeNet interaction protocol engine
 * API.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class APIVerticle extends AbstractAPIVerticle {

  /**
   * {@inheritDoc}
   */
  @Override
  protected String getOpenAPIResourcePath() {

    return "wenet-interaction_protocol_engine-openapi.yaml";
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void mountServiceInterfaces(final RouterBuilder routerFactory) {

    routerFactory.mountServiceInterface(Help.class, Help.ADDRESS);
    new ServiceBinder(this.vertx).setAddress(Help.ADDRESS).register(Help.class, new HelpResource(this));

    routerFactory.mountServiceInterface(Norms.class, Norms.ADDRESS);
    new ServiceBinder(this.vertx).setAddress(Norms.ADDRESS).register(Norms.class, new NormsResource(this.vertx));

    routerFactory.mountServiceInterface(Messages.class, Messages.ADDRESS);
    new ServiceBinder(this.vertx).setAddress(Messages.ADDRESS).register(Messages.class,
        new MessagesResource(this.vertx));

    routerFactory.mountServiceInterface(Incentives.class, Incentives.ADDRESS);
    new ServiceBinder(this.vertx).setAddress(Incentives.ADDRESS).register(Incentives.class,
        new IncentivesResource(this.vertx));

    routerFactory.mountServiceInterface(Tasks.class, Tasks.ADDRESS);
    new ServiceBinder(this.vertx).setAddress(Tasks.ADDRESS).register(Tasks.class, new TasksResource(this.vertx));

  }

  /**
   * Register the services provided by the API.
   *
   * {@inheritDoc}
   *
   * @see WeNetProfileManager
   */
  @Override
  protected void startedServerAt(final String host, final int port) {

    final var conf = new JsonObject();
    conf.put(WeNetInteractionProtocolEngineClient.INTERACTION_PROTOCOL_ENGINE_CONF_KEY, "http://" + host + ":" + port);
    final var client = WebClient.create(this.vertx);
    WeNetInteractionProtocolEngine.register(this.vertx, client, conf);

  }

}
