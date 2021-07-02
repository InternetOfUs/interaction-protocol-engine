/*
 * -----------------------------------------------------------------------------
 *
 * Copyright 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine.persistence;

import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

/**
 * A dummy implementation of the {@link NormsRepository}.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class DummyNormsRepository implements NormsRepository {

  /**
   * {@inheritDoc}
   */
  @Override
  public void searchPublishedNorm(final String id, final Handler<AsyncResult<JsonObject>> searchHandler) {

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void storePublishedNorm(final JsonObject norm, final Handler<AsyncResult<JsonObject>> storeHandler) {

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void updatePublishedNorm(final JsonObject norm, final Handler<AsyncResult<Void>> updateHandler) {

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void deletePublishedNorm(final String id, final Handler<AsyncResult<Void>> deleteHandler) {

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void retrievePublishedNormsPage(final JsonObject query, final JsonObject sort, final int offset,
      final int limit, final Handler<AsyncResult<JsonObject>> searchHandler) {

  }

}
