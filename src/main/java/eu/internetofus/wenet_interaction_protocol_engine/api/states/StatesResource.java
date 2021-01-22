/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 1994 - 2021 UDT-IA, IIIA-CSIC
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
package eu.internetofus.wenet_interaction_protocol_engine.api.states;

import eu.internetofus.common.TimeManager;
import eu.internetofus.common.components.interaction_protocol_engine.State;
import eu.internetofus.common.components.interaction_protocol_engine.StatesPage;
import eu.internetofus.common.vertx.ModelContext;
import eu.internetofus.common.vertx.ModelResources;
import eu.internetofus.common.vertx.ServiceContext;
import eu.internetofus.common.vertx.ServiceResponseHandlers;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.StatesRepository;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.api.service.ServiceRequest;
import io.vertx.ext.web.api.service.ServiceResponse;

/**
 * Implementation of the {@link States}
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class StatesResource implements States {

  /**
   * The event bus that is using.
   */
  protected Vertx vertx;

  /**
   * Create a new resource.
   *
   * @param vertx event bus to use.
   */
  public StatesResource(final Vertx vertx) {

    this.vertx = vertx;

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void retrieveStates(String communityId, String taskId, String userId, ServiceRequest context,
      Handler<AsyncResult<ServiceResponse>> resultHandler) {

    ServiceResponseHandlers.responseOk(resultHandler, new StatesPage());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void retrieveCommunityUserState(String communityId, String userId, ServiceRequest context,
      Handler<AsyncResult<ServiceResponse>> resultHandler) {

    StatesRepository.createProxy(this.vertx).searchState(communityId, null, userId).onComplete(search -> {

      var state = search.result();
      if (state == null) {

        state = new State();
        state.communityId = communityId;
        state.userId = userId;
      }

      ServiceResponseHandlers.responseOk(resultHandler, state);
    });

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void mergeCommunityUserState(String communityId, String userId, JsonObject body, ServiceRequest request,
      Handler<AsyncResult<ServiceResponse>> resultHandler) {

    final var model = new ModelContext<State, Void>();
    model.name = "state";
    model.type = State.class;
    final var context = new ServiceContext(request, resultHandler);
    ModelResources.toModel(body, model, context, () -> {

      StatesRepository.createProxy(this.vertx).searchState(communityId, null, userId).onComplete(search -> {

        var state = search.result();
        if (state == null) {

          model.source.communityId = communityId;
          model.source.userId = userId;
          model.source.taskId = null;
          model.source._creationTs = model.source._lastUpdateTs = TimeManager.now();
          StatesRepository.createProxy(this.vertx).storeState(model.source).onComplete(stored -> {

            ServiceResponseHandlers.responseOk(resultHandler, stored.result());

          });

        } else {

          for (var key : model.source.attributes.fieldNames()) {

            var value = model.source.attributes.getValue(key);
            state.attributes.put(key, value);
          }
          state._lastUpdateTs = TimeManager.now();

          StatesRepository.createProxy(this.vertx).updateState(state).onComplete(updated -> {

            ServiceResponseHandlers.responseOk(resultHandler, state);

          });
        }

      });

    });

  }

}
